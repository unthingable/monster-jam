package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api._
import com.github.unthingable.{FilteredPage, MonsterJamExt, Util}
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface._

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier
import scala.collection.mutable

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
  //j.stripBank.flushColors()

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
      case object ShiftTracking extends State
      case object Normal extends State
    }

    import JAMColorBase._
    val rainbow = Vector(RED, ORANGE, YELLOW, GREEN, LIME, CYAN, MAGENTA, FUCHSIA).map(_ * 4)

    override val modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.indices.flatMap { idx =>
      val strip: JamTouchStrip = j.stripBank.strips(idx)
      val proxy: ObjectProxy   = proxies(idx)
      val param: Parameter     = sliderParams(idx)

      var state    : State   = State.Normal

      param.markInterested()
      param.name().markInterested()
      proxy.exists().markInterested()

      var offsetObserver: Double => Unit = _ => ()
      strip.slider.value().addValueObserver(offsetObserver(_))

      var startValue: Option[Double] = None

      def engage(event: Event): Unit = {
        import Event._
        import State._

        val shiftOn = j.Modifiers.Shift.isPressed()
        val stripOn = strip.isPressed()

        state = (shiftOn, stripOn, event, state) match {
          case (_,_,ShiftP, Normal) =>
            strip.slider.clearBindings()
            ShiftTracking
          case (true, true, _:PressEvent, _) =>
            if (state == Normal)
              strip.slider.clearBindings()
            val current = param.get()
            startValue = None
            offsetObserver = { v: Double =>
              val offset = (v - startValue.getOrElse(v)) * 0.2
              param.set(current + offset)
              if (startValue.isEmpty) startValue = Some(v)
            }
            ShiftTracking
          case (true,_,StripR,ShiftTracking) =>
            offsetObserver = _ => ()
            ShiftTracking
          case (_,_,_:ReleaseEvent,ShiftTracking) =>
            offsetObserver = _ => ()
            strip.slider.setBinding(param)
            Normal
          case x: (_,_,_,_) =>
            Normal
        }

      }

      proxy.exists().addValueObserver(v => if (isOn) j.stripBank.setActive(idx, v))
      param.value().markInterested()
      param.modulatedValue().markInterested()
      param.modulatedValue().addValueObserver(127, v => if (isOn) j.stripBank.setValue(idx, v)) // move fader dot

      proxy match {
        case channel: Channel =>
          channel.color().markInterested()
          channel.color().addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
        case send: Send =>
          send.sendChannelColor().markInterested()
          send.sendChannelColor().addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
        case control: RemoteControl =>
          ()
      }

      import Event._
      Seq(
        HB(j.Modifiers.Shift.pressedAction, s"shift $idx pressed", () => engage(ShiftP), tracked = false, BindingBehavior(exclusive = false)),
        HB(j.Modifiers.Shift.releasedAction, s"shift $idx released", () => engage(ShiftR), tracked = false, BindingBehavior(exclusive = false)),
        HB(strip.pressedAction, s"shift-strip $idx pressed", () => engage(StripP), tracked = false, BindingBehavior(exclusive = false)),
        HB(strip.releasedAction, s"shift-strip $idx released", () => engage(StripR), tracked = false, BindingBehavior(exclusive = false)),
      )
    }

    private def sync(idx: Int, flush: Boolean = true): Unit = {
      j.stripBank.setValue(idx, (sliderParams(idx).value().get() * 127).intValue, flush)
    }

    override def activate(): Unit = {

      //ext.host.println(barMode.toString)
      //ext.host.println(sliderParams.map(_.name().get()).mkString(","))
      //ext.host.println(sliderParams.map(_.value().get()).mkString(","))

      j.stripBank.barMode = barMode

      j.stripBank.strips.forindex { case (strip, idx) =>
        val proxy = proxies(idx)
        if (proxy.exists().get) {
          j.stripBank.setActive(idx, value = true, flush = false)

          (proxy match {
            case channel: Channel =>
              Some(JamColorState.toColorIndex(channel.color().get()))
            case send: Send =>
              Some(JamColorState.toColorIndex(send.sendChannelColor().get()))
            case _: RemoteControl =>
              Some(rainbow(idx))
          }).foreach(c => j.stripBank.setColor(idx, c))
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

  val auxLayer = new ModeCycleLayer("aux", j.aux, CycleMode.Select) with Util {
    override val subModes = (0 to 7).map(idx =>
      new SliderBankMode[Send]("strips aux", trackBank.getItemAt(_).sendBank().getItemAt(idx), identity) {
        override val barMode: BarMode = BarMode.SINGLE
      })
  }

  val auxGate = new ModeButtonLayer("strip aux gate", j.aux,
    GateMode.Gate,
    silent = true
  ) {
    val bank = ext.host.createEffectTrackBank(8, 1)
    override val modeBindings: Seq[Binding[_, _, _]] =
      j.groupButtons.zipWithIndex.flatMap { case (btn, idx) =>
        val color = bank.getItemAt(idx).color()
        color.markInterested()

        Seq(
          HB(btn.pressedAction, s"aux select $idx", () => auxLayer.select(idx)),
          SupColorStateB(btn.light, () =>
            (if (auxLayer.selected.contains(idx))
               JamColorState(JAMColorBase.WHITE * 4, 3)
             else
               JamColorState(color.get(), 0)),
            JamColorState.empty))
    }
  }

  val levelCycle = new ModeCycleLayer("level", j.level, CycleMode.Cycle) with Util {
    override val subModes = Seq(
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
      new SliderBankMode[Track]("strips pan", trackBank.getItemAt, _.pan()) {
        override val barMode: BarMode = BarMode.PAN
      },
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
             else scene.launch()
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

    val shiftTransport = new ModeButtonLayer("shiftTransport", j.Modifiers.Shift, GateMode.Gate) {
      val loop   : SettableBooleanValue = ext.transport.isArrangerLoopEnabled
      val overdub: SettableBooleanValue = ext.transport.isClipLauncherOverdubEnabled
      val metro = ext.transport.isMetronomeEnabled
      loop.markInterested()
      overdub.markInterested()
      metro.markInterested()

      def b(button: OnOffButton, name: String, param: SettableBooleanValue) = Seq(
        HB(button.pressedAction, s"shiftTransport $name pressed", () => param.toggle()),
        SupBooleanB(button.light.isOn, param)
      )
      override val modeBindings = Seq(
        b(j.right, "loop", loop),
        b(j.record, "record", overdub),
        b(j.left, "metro", metro),
      ).flatten
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

    val solo = buttonGroupChannelMode("solo", j.solo, j.groupButtons, _.solo(), JAMColorBase.YELLOW)
    val mute = buttonGroupChannelMode("mute", j.mute, j.groupButtons, _.mute(), JAMColorBase.ORANGE)

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
      case class PressedAt(var value: Instant)

      override val modeBindings: Seq[Binding[_, _, _]] = j.matrix.indices.flatMap { col =>
        val track = trackBank.getItemAt(col)
        track.clipLauncherSlotBank().setIndication(true)
        track.isQueuedForStop.markInterested()

        val clips = track.clipLauncherSlotBank()

        val pressedAt: mutable.Seq[PressedAt] = mutable.ArraySeq.fill(8)(PressedAt(Instant.now()))

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
            HB(btn.pressedAction, s"clipPress $row:$col", () => handleClipPress(clip, clips, pressedAt(col))),
            HB(btn.releasedAction, s"clipRelease $row:$col", () => handleClipRelease(clip, clips, pressedAt(col))),
          )
        }
      } :+ HB(GlobalMode.Duplicate.deactivateAction, "dup clips: clear source", () => {
        //ext.host.println("boom")
        source = None
      }, tracked = false, behavior = BindingBehavior(managed = false))

      // for duplication
      private var source   : Option[ClipLauncherSlot] = None

      private def handleClipPress(clip: ClipLauncherSlot, clips: ClipLauncherSlotBank, pressedAt: PressedAt): Unit = {
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
          pressedAt.value = Instant.now()
        }
      }

      private def handleClipRelease(clip: ClipLauncherSlot, clips: ClipLauncherSlotBank, pressedAt: PressedAt): Unit = {
        if (Instant.now().isAfter(pressedAt.value.plus(Duration.ofSeconds(1))))
          clip.select()
        else if (clip.isPlaying.get()) clips.stop()
             else clip.launch()
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

    // devices!
    val deviceLayer = new ModeCycleLayer("device", j.control, CycleMode.Select) {
      val touchFX = "TouchFX"
      val device: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()
      val page  : FilteredPage         = FilteredPage(
        device.createCursorRemoteControlsPage(8),
        _ != touchFX)

      device.hasNext.markInterested()
      device.hasPrevious.markInterested()
      page.c.pageNames().markInterested()
      page.c.selectedPageIndex().markInterested()

      val secondCursor: CursorRemoteControlsPage = device.createCursorRemoteControlsPage(touchFX, 8, "")
      var touchPage: Option[CursorRemoteControlsPage] = None
      secondCursor.pageNames().markInterested()
      secondCursor.selectedPageIndex().markInterested()
      secondCursor.pageNames().addValueObserver(names => touchPage = names
        .zipWithIndex.find(_._1 == touchFX).map { case (_, idx) =>
          secondCursor.selectedPageIndex().set(idx)
          secondCursor
        })


      override val subModes: Seq[SubModeLayer] = Seq(
        new SliderBankMode[RemoteControl]("strips remote", page.c.getParameter, identity) {
          override val barMode: BarMode = BarMode.SINGLE

          j.stripBank.strips.forindex {case (strip, idx) =>
            strip.button.isPressed.markInterested()
            strip.button.isPressed.addValueObserver(v => touchPage.foreach(tp =>
              if (idx < tp.getParameterCount) tp.getParameter(idx).value().set(if (v) 1 else 0)))
          }
        }
      )

      def m(default: () => Boolean, modePressed: () => Boolean): BooleanSupplier =
        () => if (modeButton.isPressed()) modePressed() else default()

      def m(default: () => Unit, modePressed: () => Unit): () => Unit =
        () => if (modeButton.isPressed()) modePressed() else default()

      override val modeBindings: Seq[Binding[_, _, _]] = Seq(
        SupBooleanB(j.left.light.isOn, m(() => device.hasPrevious.get(), page.hasPrevious)),
        SupBooleanB(j.right.light.isOn, m(() => device.hasNext.get(), page.hasNext)),
        HB(j.left.pressedAction, "scroll left", m(() => device.selectPrevious(), page.selectPrevious)),
        HB(j.right.pressedAction, "scroll right", m(() => device.selectNext(), page.selectNext)),
      )

      override def activate(): Unit = {
        if (page.c.pageNames().get()(page.c.selectedPageIndex().get()) == touchFX)
          if (page.hasPrevious())
            page.selectPrevious()
          else
            page.selectNext()
        super.activate()
      }
    }

    val top    = Coexist(SimpleModeLayer("-^-", modeBindings = Seq.empty))
    val bottom = SimpleModeLayer("_|_", modeBindings = Seq.empty)
    val unmanaged = SimpleModeLayer("_x_", modeBindings = Seq.empty)
    new ModeDGraph(
      init = Seq(levelCycle),
      play -> top,
      position -> Coexist(tempoLayer),
      sceneLayer -> top,
      bottom -> Coexist(globalQuant, shiftTransport, shiftMatrix, globalShift),
      bottom -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
      trackGroup -> Exclusive(solo, mute),
      clipMatrix -> top,
      bottom -> Exclusive(levelCycle, auxLayer, deviceLayer),
      bottom -> Coexist(auxGate),
      //bottom -> Cycle(levelLayer, panLayer),
      bottom -> Coexist(unmanaged),
    )
  }

  // for now
  //ext.host.scheduleTask(() => ext.hw.invalidateHardwareOutputState(), 200)
}
