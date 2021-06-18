package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.ColorValueChangedCallback
import com.bitwig.extension.controller.api._
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface._

import java.time.{Duration, Instant}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationDouble

/*
Behavior definition for surface controls
 */

class Jam(implicit ext: MonsterJamExt) extends BindingDSL {

  val j = new JamSurface()(ext)

  object GlobalMode {
    // These only set their isOn flags and nothing else
    val Clear    : ModeButtonLayer = ModeButtonLayer("clear", j.clear, modeBindings = Seq.empty, GateMode.Gate)
    val Duplicate: ModeButtonLayer = ModeButtonLayer("duplicate", j.duplicate, modeBindings = Seq.empty, GateMode.Gate)
    val Select   : ModeButtonLayer = ModeButtonLayer("select", j.select, modeBindings = Seq.empty, GateMode.Gate)
  }

  // wire buttons

  val trackBank = ext.trackBank
  trackBank.followCursorTrack(ext.cursorTrack)

  val sceneBank  : SceneBank   = trackBank.sceneBank()
  val masterTrack: MasterTrack = ext.host.createMasterTrack(8)

  sceneBank.canScrollForwards.markInterested()
  sceneBank.canScrollBackwards.markInterested()
  sceneBank.itemCount().markInterested()

  trackBank.cursorIndex().markInterested()
  trackBank.setSkipDisabledItems(true)
  //sceneBank.setIndication(true)

  // wire strips, they are special
  j.stripBank.flushColors()

  abstract class SliderBankMode[P <: ObjectProxy](override val name: String, val obj: Int => P, val param: P => Parameter)
    extends SubModeLayer(name) with Util {
    val proxies     : Seq[P]            = j.stripBank.strips.indices.map(obj).toVector
    val sliderParams: Vector[Parameter] = proxies.map(param).toVector
    val barMode     : BarMode

    sealed trait Event
    sealed trait ShiftEvent extends Event
    sealed trait StripEvent extends Event
    sealed trait PressEvent extends Event
    sealed trait ReleaseEvent extends Event
    object Event {
      case object ShiftP extends ShiftEvent with PressEvent
      case object ShiftR extends ShiftEvent with ReleaseEvent
      case object StripP extends StripEvent with PressEvent
      case object StripR extends StripEvent with ReleaseEvent
    }

    sealed trait State
    object State {
      //case object Tracking extends State
      case object ShiftTracking extends State
      case object DoubleClicking extends State
      case object Normal extends State
    }

    override val modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.indices.flatMap { idx =>
      val strip: JamTouchStrip = j.stripBank.strips(idx)
      val proxy: ObjectProxy   = proxies(idx)
      val param: Parameter     = sliderParams(idx)

      var touchedOn: Instant = Instant.now()
      var state    : State   = State.Normal

      param.markInterested()
      param.name().markInterested()
      proxy.exists().markInterested()

      var offsetObserver: Double => Unit = _ => ()
      strip.slider.value().addValueObserver(offsetObserver(_))

      //val touchP = ext.host.createAction(() => {
      //  val current                    = param.get
      //  var startValue: Option[Double] = None
      //  strip.setOffsetCallback { v =>
      //    val offset = (v - startValue.getOrElse(v)) * 0.2
      //    param.set(current + offset)
      //    if (startValue.isEmpty) startValue = Some(v)
      //  }
      //}, () => "shift")
      //
      //val touchR = ext.host.createAction(() => strip.clearOffsetCallback(), () => "shift")
      //
      //var sbP: Option[HardwareBinding] = None
      //var sbR: Option[HardwareBinding] = None
      //
      var startValue: Option[Double] = None
      //
      //def shiftOn(): Unit = {
      //  if (strip.isPressed()) {
      //    val current = param.get()
      //    offsetObserver = { v =>
      //      val offset = (v - startValue.getOrElse(v)) * 0.2
      //      param.set(current + offset)
      //      if (startValue.isEmpty) startValue = Some(v)
      //    }
      //
      //    val min = (current - 0.1).max(0)
      //    val max = (current + 0.1).min(1)
      //    strip.slider.setBindingWithRange(sliderParams(idx), min, max)
      //  }
      //  //strip.slider.clearBindings()
      //  //sbP = Some(strip.pressedAction.setBinding(touchP))
      //  //sbR = Some(strip.releasedAction.setBinding(touchR))
      //}
      //
      //def shiftOff(): Unit = {
      //  strip.clearOffsetCallback()
      //  sbP.foreach(_.removeBinding())
      //  sbR.foreach(_.removeBinding())
      //  //if (strip.isPressed())
      //    strip.slider.setBinding(sliderParams(idx))
      //}
      //
      //def touchOn(): Unit = {
      //  isTouched = true
      //  val now = Instant.now()
      //  if (now.isBefore(touchedOn.plus(Duration.ofMillis(500)))) { // doubleclick
      //    strip.slider.clearBindings()
      //    ext.host.println(s"doubleclick $idx")
      //    sliderParams(idx).reset()
      //    j.stripBank.setValue(idx, (sliderParams(idx).value().get * 128).intValue)
      //    var b: HardwareBinding = null
      //    b = strip.releasedAction.setBinding(action("after double", () => {
      //
      //      b.removeBinding()
      //    }))
      //  } else {
      //    //strip.slider.setBinding(sliderParams(idx))
      //  }
      //  touchedOn = now
      //}
      //
      //def touchOff(): Unit = {
      //  isTouched = false
      //  //strip.slider.clearBindings()
      //  // restore after doubleclick
      //  strip.slider.setBinding(sliderParams(idx))
      //  // ignore funny business
      //  j.stripBank.setValue(idx, (sliderParams(idx).value().get * 128).intValue)
      //}
      //
      //
      //var doubleClickWait: Boolean = false
      //
      //def engageTracking(): Unit = {
      //  (j.Modifiers.Shift.isPressed(), strip.isPressed()) match {
      //    case (true, true) =>
      //      strip.slider.clearBindings()
      //      val current = param.get()
      //      offsetObserver = { v =>
      //        val offset = (v - startValue.getOrElse(v)) * 0.2
      //        param.set(current + offset)
      //        if (startValue.isEmpty) startValue = Some(v)
      //      }
      //    case (false, true) =>
      //      strip.slider.setBinding(param)
      //    case _ => ()
      //  }
      //}

      def engage(event: Event): Unit = {
        import Event._
        import State._

        val shiftOn = j.Modifiers.Shift.isPressed()
        val stripOn = strip.isPressed()

        def doubleClicked = if (event == StripP) {
          val now = Instant.now()
          val prior = touchedOn
          touchedOn = now
          now.isBefore(prior.plus(Duration.ofMillis(800)))
        } else false

        state = (shiftOn, stripOn, event, state) match {
          case (_, _, StripR, _) =>
            strip.slider.setBinding(param)
            sync(idx)
            Normal
          case (false, false, _, _) =>
            Normal
          case (false,_,StripP,_) if doubleClicked =>
            strip.slider.clearBindings()
            ext.host.println(s"doubleclick $idx")
            sliderParams(idx).reset()
            //j.stripBank.setValue(idx, (sliderParams(idx).value().get * 128).intValue)
            DoubleClicking
          case (true, true, _:PressEvent, _) =>
            strip.slider.clearBindings()
            val current = param.get()
            startValue = None
            offsetObserver = { v: Double =>
              val offset = (v - startValue.getOrElse(v)) * 0.2
              param.set(current + offset)
              if (startValue.isEmpty) startValue = Some(v)
            }
            ShiftTracking
          case (_,_,_:ReleaseEvent,ShiftTracking) =>
            offsetObserver = _ => ()
            strip.slider.setBinding(param)
            Normal
          case (_,_,_,_) => Normal
        }

      }

      //def disengage(): Unit = {
      //  (j.Modifiers.Shift.isPressed(), strip.isPressed()) match {
      //    case (false, ) =>
      //
      //}

      //j.Modifiers.Shift.pressedAction.addBinding(action(s"shit-strips $i pressed", { () =>
      //  strip.slider.clearBindings()
      //  strip.pressedAction.setBinding(touchP)
      //  strip.releasedAction.setBinding(touchR)
      //  ()
      //}))
      //j.Modifiers.Shift.releasedAction.addBinding(action(s"shift-strips $i released", { () =>
      //  strip.clearOffsetCallback()
      //  strip.pressedAction.clearBindings()
      //  strip.releasedAction.clearBindings()
      //  strip.slider.setBinding(sliderParams(i))
      //  ()
      //}))

      proxy.exists().addValueObserver(v => if (isOn) j.stripBank.setActive(idx, v))
      param.value().markInterested()
      param.value().addValueObserver(128, v => if (isOn) j.stripBank.setValue(idx, v)) // move fader dot

      proxy match {
        case channel: Channel =>
          channel.color().markInterested()
          channel.color().addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
      }

      import Event._
      Seq(
        //HB(j.Modifiers.Shift.pressedAction, s"shift-strip $idx pressed", () => shiftOn(), tracked = false, BindingBehavior(exclusive = false)),
        //HB(j.Modifiers.Shift.releasedAction, s"shift-strip $idx released", () => shiftOff(), tracked = false, BindingBehavior(exclusive = false)),
        //HB(strip.pressedAction, s"shift-strip $idx pressed", () => touchOn(), tracked = false, BindingBehavior(exclusive = false)),
        //HB(strip.releasedAction, s"shift-strip $idx released", () => touchOff(), tracked = false, BindingBehavior(exclusive = false)),
        HB(j.Modifiers.Shift.pressedAction, s"shift $idx pressed", () => engage(ShiftP), tracked = false, BindingBehavior(exclusive = false)),
        HB(j.Modifiers.Shift.releasedAction, s"shift $idx released", () => engage(ShiftR), tracked = false, BindingBehavior(exclusive = false)),
        HB(strip.pressedAction, s"shift-strip $idx pressed", () => engage(StripP), tracked = false, BindingBehavior(exclusive = false)),
        HB(strip.releasedAction, s"shift-strip $idx released", () => engage(StripR), tracked = false, BindingBehavior(exclusive = false)),
      )
    }


    private def sync(idx: Int, flush: Boolean = true): Unit = {
      j.stripBank.setValue(idx, (sliderParams(idx).value().get() * 128).intValue, flush)
    }

    override def activate(): Unit = {

      ext.host.println(barMode.toString)
      ext.host.println(sliderParams.map(_.name().get()).mkString(","))
      //ext.run(List.fill(8)((100, () => true, () => )))
      ext.host.println(sliderParams.map(_.value().get()).mkString(","))

      j.stripBank.barMode = barMode

      j.stripBank.strips.forindex { case (strip, idx) =>
        val proxy = proxies(idx)
        if (proxy.exists().get) {
          j.stripBank.setActive(idx, value = true, flush = false)

          proxy match {
            case channel: Channel =>
              j.stripBank.setColor(idx, JamColorState.toColorIndex(channel.color().get()))
          }
        } else {
          j.stripBank.setActive(idx, value = false, flush = false)
        }
      }

      j.stripBank.flushColors()

      sliderParams.indices.foreach(sync(_, false))
      if (barMode == BarMode.DUAL)
        j.stripBank.flushValues()

      j.stripBank.strips.zip(sliderParams).foreach { case (s, p) => s.slider.setBinding(p)}

      super.activate()
    }

    override def deactivate(): Unit = {
      j.stripBank.strips.foreach(_.slider.clearBindings())
      super.deactivate()
    }
  }

  //def stripLayer(name: String, button: JamOnOffButton) = new ModeButtonLayer(name, button) {
  //  override val modeBindings: Seq[Binding[_, _, _]] = Seq.empty
  //
  //
  //}

  //val levelLayer = new ModeButtonLayer("strips level", j.level, gateMode = GateMode.OneWay) {
  //  override val modeBindings: Seq[Binding[_, _, _]] = Seq.empty
  //    //j.stripBank.strips.zipWithIndex.map { case (strip, idx) =>
  //    //  val track = trackBank.getItemAt(idx)
  //    //  ValObserverB[ColorValueChangedCallback, JamTouchStrip](
  //    //    track.color(),
  //    //    (r, g, b) => j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)),
  //    //    strip)
  //    //}
  //
  //  override def activate(): Unit = {
  //    super.activate()
  //    trackTrackColor = true
  //    //ext.host.println("+++ activated")
  //  }
  //
  //  override def deactivate(): Unit = {
  //    trackTrackColor = false
  //    super.deactivate()
  //  }
  //}
  //
  //val panLayer = new ModeButtonLayer("strips pan", j.level, gateMode = GateMode.OneWay) {
  //  override val modeBindings: Seq[Binding[_, _, _]] = Seq.empty
  //}

  val auxLayer = new ModeButtonLayer("strips aux", j.aux, gateMode = GateMode.OneWay) with Util {
    override val modeBindings: Seq[Binding[_, _, _]] = Seq.empty
      //j.stripBank.strips.zipWithIndex.map { case (strip, idx) =>
      //  ValObserverB[ColorValueChangedCallback, JamTouchStrip](
      //    () => toColor(Color.WHITE),
      //    (r, g, b) => j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)),
      //    strip)
      //}

    override def activate(): Unit = {
      super.activate()
      j.stripBank.strips.indices.foreach { idx =>
        j.stripBank.setColor(idx, JamColorState.toColorIndex(toColor(java.awt.Color.WHITE)))
      }
    }
  }

  val auxGate = ModeButtonLayer("strip aux gate", j.aux,
    j.groupButtons.zipWithIndex.map { case (btn, idx) =>
      HB(btn.pressedAction, s"aux select $idx", () => ())
    },
    GateMode.Gate,
    silent = true
  )

  val levelCycle = new ModeCycleLayer("level", j.level, GateMode.OneWay) with Util {
    override val subModes = Seq(
      new SliderBankMode[Track]("strips pan", trackBank.getItemAt, _.pan()) {
        override val barMode: BarMode = BarMode.PAN
      },
      new SliderBankMode[Track]("strips volume", trackBank.getItemAt, _.volume()) {
        override val barMode: BarMode = BarMode.DUAL

        proxies.forindex { case(track, idx) =>
          val strip: JamTouchStrip = j.stripBank.strips(idx)
          track.addVuMeterObserver(128, -1, true, v => if (isOn) strip.update(v))
        }

        override def activate(): Unit = {
          super.activate()
          j.stripBank.strips.foreach(_.update(0))
        }
      },
      //new SubModeLayer("strips volume") {
      //  override def activate(): Unit = {
      //    super.activate()
      //    trackTrackColor = true
      //    j.stripBank.strips.indices.foreach { idx =>
      //      sliderParams.update(idx, trackBank.getItemAt(idx).volume())
      //    }
      //  }
      //
      //  override def deactivate(): Unit = {
      //    trackTrackColor = false
      //    super.deactivate()
      //  }
      //
      //  override val modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.zipWithIndex.map { case(strip, idx) =>
      //    HB(strip.slider, s"strip volume $idx", trackBank.getItemAt(idx).volume())
      //  }
      //},
      //new SubModeLayer("strips pan") {
      //  j.stripBank.strips.indices.foreach(trackBank.getItemAt(_).pan().markInterested())
      //  override def activate(): Unit = {
      //    super.activate()
      //    j.stripBank.strips.indices.foreach { idx =>
      //      j.stripBank.setColor(idx, JamColorState.toColorIndex(toColor(java.awt.Color.WHITE)))
      //      sliderParams.update(idx, trackBank.getItemAt(idx).pan())
      //    }
      //  }
      //
      //  //override def deactivate(): Unit = super.deactivate()
      //},
    )
  }

  // this behavior ls always the same, can wire it here directly without creating a mode layer
  {
    // wire dpad
    Seq(
      j.dpad.left -> trackBank.canScrollBackwards,
      j.dpad.right -> trackBank.canScrollForwards,
      j.dpad.up -> sceneBank.canScrollBackwards,
      j.dpad.down -> sceneBank.canScrollForwards
    ) foreach { case (b: JamOnOffButton, e: BooleanValue) =>
      e.markInterested()
      b.light.isOn.setValueSupplier(e)
    }

    def scroll(forward: Boolean, target: Scrollable): HardwareActionBindable = {
      ext.host.createAction(() =>
        (j.Modifiers.Shift.isPressed(), forward) match {
          case (false, true)  => target.scrollPageForwards()
          case (false, false) => target.scrollPageBackwards()
          case (true, true)   => target.scrollForwards()
          case (true, false)  => target.scrollBackwards()
        }, () => s"scroll_$forward")
    }

    j.dpad.left.pressedAction.setBinding(scroll(false, trackBank))
    j.dpad.right.pressedAction.setBinding(scroll(true, trackBank))
    j.dpad.up.pressedAction.setBinding(scroll(false, sceneBank))
    j.dpad.down.pressedAction.setBinding(scroll(true, sceneBank))

    // meters
    masterTrack.addVuMeterObserver(128, 0, true, j.levelMeter.uL)
    masterTrack.addVuMeterObserver(128, 1, true, j.levelMeter.uR)
  }


  // Mode layers

  {
    // wire scene buttons
    val sceneLayer = new SimpleModeLayer("scene") {
      override val modeBindings: Seq[Binding[_, _, _]] = j.sceneButtons.indices.flatMap { i =>
        val btn  : JamRgbButton = j.sceneButtons(i)
        val scene: Scene        = sceneBank.getScene(i)
        scene.color.markInterested()
        scene.exists.markInterested()

        Seq(
          SupColorB(btn.light, scene.color()),
          HB(btn.pressedAction, s"scene $i press", () => handlePress(scene)))
      }

      private def handlePress(scene: Scene): Unit = {
        if (GlobalMode.Clear.isOn) scene.deleteObject()
        else if (GlobalMode.Duplicate.isOn) scene.nextSceneInsertionPoint().copySlotsOrScenes(scene)
             else scene.launchAction().invoke()
      }
    }

    val position = SimpleModeLayer("position",
      Seq(HB(j.encoder.turn, "position turn", ext.host.createRelativeHardwareControlStepTarget(
        ext.transport.fastForwardAction(),
        ext.transport.rewindAction()))))

    val tempoLayer = ModeButtonLayer("tempo",
      j.tempo,
      Seq(
        HB(j.encoder.turn, "tempo turn", ext.host.createRelativeHardwareControlStepTarget(
          action("inc tempo", () => ext.transport.increaseTempo(1, 647)),
          action("dec tempo", () => ext.transport.increaseTempo(-1, 647))))))

    val play = new SimpleModeLayer("play") {
      ext.transport.isPlaying.markInterested()

      val playPressAction: HardwareActionBindable = action(s"$name play pressed", () => {
        val isPlaying = ext.transport.isPlaying
        val t         = ext.transport
        (isPlaying.get(), j.Modifiers.Shift.isPressed()) match {
          // needs work
          // just play
          case (true, false) => t.play()
          // restart (and stop)
          case (true, true) => t.restart()
          // resume
          case (false, false) => t.continuePlayback()
          case (false, true)  => restart(false)
        }
      })

      def restart(go: Boolean): Unit = {
        val h = ext.host
        ext.transport.stop()
        h.scheduleTask(() => {
          ext.transport.stop()
          h.scheduleTask(() => {
            ext.transport.stop()
            if (go) h.scheduleTask(() => ext.transport.play(), 10)
          }, 10)
        }, 10)
      }

      override val modeBindings = Seq(
        HB(j.play.pressedAction, "play pressed", playPressAction, tracked = false),
        SupBooleanB(j.play.light.isOn, ext.transport.isPlaying),
      )
    }

    val loop = new ModeButtonLayer("loop", j.Modifiers.Shift, GateMode.Gate) {
      val loop = ext.transport.isArrangerLoopEnabled
      loop.markInterested()

      override val modeBindings = Seq(
        HB(j.right.pressedAction, "loop pressed", () => loop.toggle()),
        SupBooleanB(j.right.light.isOn, loop)
      )
    }

    val globalQuant = new ModeButtonLayer("globalQuant",
      modeButton = j.grid,
      gateMode = GateMode.Gate
    ) {
      val quant = ext.transport.defaultLaunchQuantization()
      quant.markInterested()
      val enumValues = Vector("8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16")
      override val modeBindings = j.sceneButtons.indices.flatMap { idx =>
        val sceneButton = j.sceneButtons(idx)

        Seq(
          SupColorB(sceneButton.light, () =>
            if (quant.get() == enumValues(idx)) Color.whiteColor() else Color.blackColor()),
          HB(sceneButton.pressedAction, "global quant grid", action(s"grid $idx", () => {
            if (quant.get == enumValues(idx))
              quant.set("none")
            else
              quant.set(enumValues(idx))
          })))
      }
    }

    def buttonGroupChannelMode(
      name: String,
      modeButton: JamOnOffButton,
      group: Seq[JamRgbButton],
      prop: Channel => SettableBooleanValue,
      color: Int // Jam's color index
    ): ModeButtonLayer = ModeButtonLayer(name, modeButton, group.indices.flatMap { idx =>
        val track       = trackBank.getItemAt(idx)
        val propValue   = prop(track)
        val existsValue = track.exists()
        propValue.markInterested()
        existsValue.markInterested()
        val gButton = group(idx)

        Seq(
          HB(gButton.pressedAction, s"group $idx pressed: $name", () => propValue.toggle()),
          SupColorStateB(gButton.light, () => {
            (existsValue.get(), propValue.get()) match {
              case (false, _) => JamColorState.empty
              case (_, false) => JamColorState(color * 4, 0)
              case (_, true)  => JamColorState(color * 4, 3)
            }
          }, JamColorState.empty)
        )
      }
    )

    val solo       = buttonGroupChannelMode("solo", j.solo, j.groupButtons, _.solo(), JAMColorBase.YELLOW)
    val mute       = buttonGroupChannelMode("mute", j.mute, j.groupButtons, _.mute(), JAMColorBase.ORANGE)

    val trackGroup = new SimpleModeLayer("trackGroup") {
      override val modeBindings: Seq[Binding[_, _, _]] = j.groupButtons.indices flatMap { idx =>
        val btn = j.groupButtons(idx)

        val track        = trackBank.getItemAt(idx)
        val color        = track.color()
        val cursorIndex  = trackBank.cursorIndex()
        val playingNotes = track.playingNotes()

        color.markInterested()
        playingNotes.markInterested()
        cursorIndex.markInterested()
        track.exists().markInterested()

        Seq(
          SupColorStateB(btn.light, () => JamColorState(
            color.get(),
            brightness = (playingNotes.get().length > 0, cursorIndex.get() == idx) match {
              case (_, true)  => 3
              case (true, _)  => 2
              case (false, _) => 0
            }
          ), JamColorState.empty),
          //HB(btn.button.pressedAction(), () => trackBank.cursorIndex().set(idx))
          HB(btn.pressedAction, s"group $idx pressed: select in mixer", () => handlePress(track))
        )
      }
      private def handlePress(track: Track): Unit = {
        if (GlobalMode.Clear.isOn) track.deleteObject()
        else if (GlobalMode.Duplicate.isOn) track.duplicate()
        else track.selectInMixer()
      }
    }

    /**
     * Default clip matrix with clip launchers
     */
    val clipMatrix = new SimpleModeLayer("clipMatrix") {

      override val modeBindings: Seq[Binding[_, _, _]] = j.matrix.indices.flatMap { col =>
        val track = trackBank.getItemAt(col)
        track.clipLauncherSlotBank().setIndication(true)
        track.isQueuedForStop.markInterested()

        val clips = track.clipLauncherSlotBank()
        (0 to 7).flatMap { row =>
          val btn  = j.matrix(row)(col)
          val clip = clips.getItemAt(row)
          clip.color().markInterested()
          clip.isPlaying.markInterested()
          clip.isSelected.markInterested()
          clip.isPlaybackQueued.markInterested()
          clip.isStopQueued.markInterested()
          clips.exists().markInterested()

          Seq(
            SupColorStateB(btn.light, () => clipColor(track, clip), JamColorState.empty),
            HB(btn.pressedAction, s"clipPress $row:$col", () => handleClipPress(clip, clips)),
            HB(btn.releasedAction, s"clipRelease $row:$col", () => handleClipRelease(clip, clips)),
          )
        }
      } :+ HB(GlobalMode.Duplicate.deactivateAction, "dup clips: clear source", () => {
        //ext.host.println("boom")
        source = None
      }, tracked = false, behavior = BindingBehavior(managed = false))

      // for duplication
      private var source   : Option[ClipLauncherSlot] = None
      private var pressed  : Option[ClipLauncherSlot] = None
      private var pressedAt: Instant = Instant.now() // initial value doesn't matter

      private def handleClipPress(clip: ClipLauncherSlot, clips: ClipLauncherSlotBank): Unit = {
        if (GlobalMode.Select.isOn) clip.select()
        else if (GlobalMode.Clear.isOn) clip.deleteObject()
        else if (GlobalMode.Duplicate.isOn) {
          if (source.isEmpty) source = Some(clip)
          else {
            source.foreach(s => if (s != clip) clip.replaceInsertionPoint().copySlotsOrScenes(s))
            source = None
          }
        }
         else {
          pressed = Some(clip)
          pressedAt = Instant.now()
        }
      }

      private def handleClipRelease(clip: ClipLauncherSlot, clips: ClipLauncherSlotBank): Unit = {
        if (pressed.isDefined) {
          val elapsed = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
          if (elapsed)
            clip.select()
          else if (clip.isPlaying.get()) clips.stop()
               else clip.launch()
        }
        pressed = None
      }

      private def clipColor(track: Track, clip: ClipLauncherSlot): JamColorState = {
        if (GlobalMode.Select.isOn && clip.isSelected.get())
          JamColorState(JAMColorBase.WHITE * 4, 3)
        else if (!GlobalMode.Select.isOn && source.contains(clip))
          JamColorState(JAMColorBase.WHITE * 4, if (j.Modifiers.blink) 3 else 1)
        else
          JamColorState(
            clip.color().get(),
            brightness = {
              if (clip.isPlaying.get())
                if (track.isQueuedForStop.get()) if (j.Modifiers.blink) 3 else -1
                else 3
              else if (clip.isPlaybackQueued.get()) if (j.Modifiers.blink) 0 else 3
                   else 0
            }
          )
      }
    }

    /**
     * Shift matrix row (like Moss)
     */
    val shiftMatrix = new ModeButtonLayer("shiftMatrix", j.Modifiers.Shift, GateMode.Gate) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] = Seq(
        (JAMColorBase.RED, () => ext.application.undo()),
        (JAMColorBase.GREEN, () => ext.application.redo()),
        (JAMColorBase.LIME, () => clip.quantize(1.0)),
        (JAMColorBase.LIME, () => clip.quantize(0.5)),
        (JAMColorBase.MAGENTA, () => clip.transpose(-1)),
        (JAMColorBase.MAGENTA, () => clip.transpose(1)),
        (JAMColorBase.FUCHSIA, () => clip.transpose(-12)),
        (JAMColorBase.FUCHSIA, () => clip.transpose(12)),
      ).zipWithIndex.flatMap { case ((color, action), idx) =>
        val button = j.matrix(0)(idx)
        Seq(
          HB(button.pressedAction, s"shift-$idx matrix pressed", action),
          SupColorStateB(
            button.light, () => JamColorState(
              color * 4,
              brightness = if (button.isPressed()) 2 else 0),
            JamColorState.empty)
        )
      }
    }

    val globalShift = new ModeButtonLayer("globalShift", j.Modifiers.Shift, GateMode.Gate) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] = Seq(
        HB(j.duplicate.pressedAction, "shift dup clip content", () => clip.duplicateContent())
      )
    }

    val top    = Coexist(SimpleModeLayer("-^-", modeBindings = Seq.empty))
    val bottom = SimpleModeLayer("_|_", modeBindings = Seq.empty)
    val unmanaged = SimpleModeLayer("_x_", modeBindings = Seq.empty)
    new ModeDGraph(
      init = Seq(levelCycle),
      play -> top,
      position -> Coexist(tempoLayer),
      sceneLayer -> top,
      bottom -> Coexist(globalQuant, loop, shiftMatrix, globalShift),
      bottom -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
      trackGroup -> Exclusive(solo, mute),
      clipMatrix -> top,
      bottom -> Exclusive(levelCycle, auxLayer),
      bottom -> Coexist(auxGate),
      //bottom -> Cycle(levelLayer, panLayer),
      bottom -> Coexist(unmanaged),
    )
  }

  // for now
  //ext.host.scheduleTask(() => ext.hw.invalidateHardwareOutputState(), 200)
}
