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
import scala.util.Try

/*
Behavior definition for surface controls
 */

class Jam(implicit ext: MonsterJamExt) extends BindingDSL {

  val j = new JamSurface()(ext)

  val EIGHT: Vector[Int] = (0 to 7).toVector

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
    val proxies     : Vector[P]            = j.stripBank.strips.indices.map(obj).toVector
    val sliderParams: Vector[Parameter] = proxies.map(param)
    val barMode: BarMode

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

    val rainbow = Vector(RED, ORANGE, YELLOW, GREEN, LIME, CYAN, MAGENTA, FUCHSIA)

    override val modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.indices.flatMap { idx =>
      val strip: JamTouchStrip = j.stripBank.strips(idx)
      val proxy: ObjectProxy   = proxies(idx)
      val param: Parameter     = sliderParams(idx)

      var state: State = State.Normal

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
        case channel: Channel       =>
          channel.color().markInterested()
          channel.color().addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
        case send: Send             =>
          send.sendChannelColor().markInterested()
          send.sendChannelColor().addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
        case _: RemoteControl =>
          ()
      }

      import Event._
      Vector(
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
            case send: Send       =>
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

      j.stripBank.strips.zip(sliderParams).foreach { case (s, p) => s.slider.setBinding(p) }

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

        Vector(
          HB(btn.pressedAction, s"aux select $idx", () => auxLayer.select(idx)),
          SupColorStateB(btn.light, () =>
            (if (auxLayer.selected.contains(idx))
               JamColorState(JAMColorBase.WHITE, 3)
             else
               JamColorState(color.get(), 0)),
            JamColorState.empty))
      }
  }

  val levelCycle = new ModeCycleLayer("level", j.level, CycleMode.Cycle) with Util {
    override val subModes = Vector(
      new SliderBankMode[Track]("strips volume", trackBank.getItemAt, _.volume()) {
        override val barMode: BarMode = BarMode.DUAL

        proxies.forindex { case (track, idx) =>
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
    Vector(
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

        Vector(
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
      Vector(HB(j.encoder.turn, "position turn", ext.host.createRelativeHardwareControlStepTarget(
              ext.transport.fastForwardAction(),
              ext.transport.rewindAction()))))

    val tempoLayer = ModeButtonLayer("tempo",
      j.tempo,
      Vector(
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

      override val modeBindings = Vector(
        HB(j.play.pressedAction, "play pressed", playPressAction, tracked = false),
        SupBooleanB(j.play.light.isOn, ext.transport.isPlaying),
      )
    }

    val shiftTransport = new ModeButtonLayer("shiftTransport", j.Modifiers.Shift, GateMode.Gate) {
      val loop   : SettableBooleanValue = ext.transport.isArrangerLoopEnabled
      val overdub: SettableBooleanValue = ext.transport.isClipLauncherOverdubEnabled
      val metro                         = ext.transport.isMetronomeEnabled
      loop.markInterested()
      overdub.markInterested()
      metro.markInterested()

      def b(button: OnOffButton, name: String, param: SettableBooleanValue) = Vector(
        HB(button.pressedAction, s"shiftTransport $name pressed", () => param.toggle()),
        SupBooleanB(button.light.isOn, param)
      )

      override val modeBindings = Vector(
        b(j.right, "loop", loop),
        b(j.record, "record", overdub),
        b(j.left, "metro", metro),
      ).flatten ++ Vector(
        HB(j.tempo.pressedAction, "tap tempo", ext.transport.tapTempoAction())
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

        Vector(
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

      Vector(
        HB(gButton.pressedAction, s"group $idx pressed: $name", () => propValue.toggle()),
        SupColorStateB(gButton.light, () => {
          (existsValue.get(), propValue.get()) match {
            case (false, _) => JamColorState.empty
            case (_, false) => JamColorState(color, 0)
            case (_, true)  => JamColorState(color, 3)
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

        Vector(
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

          Vector(
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
      private var source: Option[ClipLauncherSlot] = None

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
          JamColorState(JAMColorBase.WHITE, 3)
        else if (!GlobalMode.Select.isOn && source.contains(clip))
               JamColorState(JAMColorBase.WHITE, if (j.Modifiers.blink) 3 else 1)
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

    var superScene = new ModeButtonLayer("superScene", j.song, GateMode.Toggle) with Util {
      val maxTracks = 64 // can be up to 256 before serialization needs to be rethought
      val maxScenes = 64
      val bufferSize = maxTracks * maxScenes * 4
      var pageIndex = 0
      var lastScene: Option[Int] = None

      val superBank  : TrackBank                  = ext.host.createMainTrackBank(maxTracks, 8, maxScenes)
      superBank.itemCount().markInterested()

      val setting: SettableStringValue = ext.document.getStringSetting("superScene", "MonsterJam", bufferSize, "")
      setting.asInstanceOf[Setting].hide()

      lazy val superScenes: mutable.ArraySeq[Map[Int, Int]] = mutable.ArraySeq.from(fromSettings(setting.get()))

      private def fromSettings(s: String): Iterable[Map[Int, Int]] =
        Try(deserialize(maxTracks, maxScenes)(s))
          .toEither
          .filterOrElse(_.nonEmpty, new Exception("Deserialized empty"))
          .left.map { e => ext.host.println(s"Failed to deserialize superscenes: ${e}"); e }
          .getOrElse(Vector.fill(maxTracks)(Map.empty))

      ext.application.projectName().markInterested()
      ext.application.projectName().addValueObserver(_ => {
        fromSettings(setting.get()).forindex {case (m, idx) => superScenes.update(idx, m)}
      })

      (0 until maxTracks).foreach {t =>
        val clips = superBank.getItemAt(t).clipLauncherSlotBank()
        clips.itemCount().markInterested()
        (0 until maxScenes).foreach(c => clips.getItemAt(c).isPlaying.markInterested())
      }

      case class ClipTarget(track: Int, clip: Int)

      def scan(): Seq[ClipTarget] = (0 until maxTracks.min(superBank.itemCount().get)).flatMap { tIdx =>
        val scenes = superBank.getItemAt(tIdx).clipLauncherSlotBank()
        (0 until maxScenes.min(scenes.itemCount().get())).flatMap { sIdx =>
          val clip = scenes.getItemAt(sIdx)

          if (clip.isPlaying.get())
            Some(ClipTarget(tIdx, sIdx))
          else
            None
        }
      }

      def recall(scene: Int): Unit = {
        (0 until maxTracks).foreach { track =>
          superScenes(scene).get(track) match {
            case Some(clip) =>
              superBank.getItemAt(track).clipLauncherSlotBank().launch(clip)
            case None        =>
              superBank.getItemAt(track).stop()
          }
          lastScene = Some(scene)
        }
      }

      def pressed(scene: Int): Unit = {
        if (GlobalMode.Clear.isOn) superScenes.update(scene, Map.empty)
        else if (superScenes(scene).isEmpty) {
          superScenes.update(scene, scan().map(ct => ct.track -> ct.clip).toMap)
          lastScene = Some(scene)

          val ser = serialize(maxTracks, maxScenes)(superScenes)
          //ext.host.println(ser)
          setting.set(ser)
        } else
            recall(scene)
      }

      def serialize(rows: Int, cols: Int)(o: Iterable[Map[Int, Int]]): String =
        o.take(rows).map { row =>
          (0 until cols).map { idx =>
            f"${idx}%02x${row.get(idx).map(_ + 1).getOrElse(0)}%02x"
          }.mkString
        }.mkString

      def deserialize(rows: Int, cols: Int)(s: String): Iterable[Map[Int, Int]] = {
        assert(s.length == bufferSize, s"length mismatch: expected $bufferSize, got ${s.length}")
        assert(rows * cols * 4 == bufferSize, s"rows * cols (${rows * cols * 4}) does not match expected bufferSize $bufferSize")
        val ret = s.grouped(2).map(Integer.parseInt(_, 16))
          .grouped(2).map(s => s(0) -> (s(1) - 1))
          .grouped(cols).map(_.filter(_._2 != -1).toMap)
          .toVector
        assert(ret.forall(_.forall(x => x._1 < maxTracks && x._2 < maxScenes)), "index out of bounds")
        ret
      }

      def page(idx: Int): mutable.Seq[Map[Int, Int]] = superScenes.slice(idx * 8, (idx + 1) * 8)

      override val modeBindings: Seq[Binding[_, _, _]] = (0 to 7).flatMap { idx =>
        def pageOffset = pageIndex * 8
        Vector(
          HB(j.sceneButtons(idx).pressedAction, s"super scene $idx pressed", () => pressed(pageOffset + idx)),
          SupColorStateB(j.sceneButtons(idx).light, () =>
            JamColorState(
              if (superScenes(pageOffset + idx).isEmpty)
                JAMColorBase.OFF
              else if (lastScene.contains(pageOffset + idx)) JAMColorBase.WHITE
              else (((pageOffset + idx) % 16) + 1) * 4,
              if (lastScene.contains(pageOffset + idx)) 2 else 0), JamColorState.empty),
        )
      }
    }

    /**
     * Shift matrix row
     */
    val shiftMatrix = new ModeButtonLayer("shiftMatrix", j.Modifiers.Shift, GateMode.Gate) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] = Vector(
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
        Vector(HB(button.pressedAction, s"shift-$idx matrix pressed", action)) ++ (
          if (ext.preferences.shiftRow.get())
            Vector(SupColorStateB(
              button.light, () => JamColorState(
                color,
                brightness = if (button.isPressed()) 2 else 0),
              JamColorState.empty))
          else Vector.empty
          )
      }
    }

    val shiftPages = new ModeCycleLayer("shiftMatrix", j.Modifiers.Shift, CycleMode.GateSelect) {
      trackBank.itemCount().markInterested()
      trackBank.scrollPosition().markInterested()
      sceneBank.itemCount().markInterested()
      sceneBank.scrollPosition().markInterested()
      ext.preferences.shiftGroup.markInterested()

      def bankB(b: Bank[_], btn: Int => JamRgbButton, idx: Int) = Vector(
        SupColorStateB(btn(idx).light, () =>
          if (b.itemCount().get() > idx * 8)
            if (((idx * 8) - 7 until (idx * 8) + 7) contains b.scrollPosition().get())
              JamColorState(JAMColorBase.WHITE, 2)
            else
              JamColorState(JAMColorBase.WARM_YELLOW, 0)
          else JamColorState.empty
          , JamColorState.empty),
        HB(btn(idx).pressedAction, "shift-scroll page $idx", () => b.scrollPosition().set(idx * 8))
      )

      val trackPages: Vector[Binding[_, _, _]] =
        if (ext.preferences.shiftGroup.get())
          EIGHT.flatMap(bankB(trackBank, j.groupButtons, _))
        else
          Vector.empty

      override val subModes = Vector(
        new SimpleModeLayer("scene pages") with IntActivatedLayer {
          override val modeBindings: Vector[Binding[_, _, _]] =
            EIGHT.flatMap(bankB(sceneBank, j.sceneButtons, _)) ++ trackPages
        },
        new SimpleModeLayer("superscene pages") with IntActivatedLayer {
          override val modeBindings: Vector[Binding[_, _, _]] = {
            trackPages ++
            (0 to 7).flatMap(idx => bankB(sceneBank, j.matrix(7), idx) ++ Vector(
              SupColorStateB(j.sceneButtons(idx).light, () =>
                if (idx == superScene.pageIndex)
                  JamColorState(JAMColorBase.WHITE, 3)
                else if (superScene.lastScene.exists(i => (0 until 8).map(_ + (idx * 8)).contains(i)))
                  JamColorState(JAMColorBase.LIME, 0)
                else if (superScene.page(idx).exists(_.nonEmpty))
                  JamColorState(JAMColorBase.ORANGE, 0)
                else JamColorState.empty
              , JamColorState.empty),
              HB(j.sceneButtons(idx).pressedAction, "super scene page $idx", () => superScene.pageIndex = idx)
            ))
          }
        }
      )

      override def activate(): Unit = {
        selected = if (superScene.isOn) Some(1) else Some(0)
        super.activate()
      }
    }

    val globalShift = new ModeButtonLayer("globalShift", j.Modifiers.Shift, GateMode.Gate) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] = Vector(
        HB(j.duplicate.pressedAction, "shift dup clip content", () => clip.duplicateContent())
      )
    }

    // devices!
    val deviceLayer = new ModeCycleLayer("device", j.control, CycleMode.Select) {
      val touchFX                      = "MonsterFX"
      val device: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()
      val page  : FilteredPage         = FilteredPage(
        device.createCursorRemoteControlsPage(8),
        _ != touchFX)

      device.hasNext.markInterested()
      device.hasPrevious.markInterested()
      page.c.pageNames().markInterested()
      page.c.selectedPageIndex().markInterested()

      val secondCursor: CursorRemoteControlsPage         = device.createCursorRemoteControlsPage(touchFX, 8, "")
      var touchPage   : Option[CursorRemoteControlsPage] = None
      secondCursor.pageNames().markInterested()
      secondCursor.selectedPageIndex().markInterested()
      secondCursor.pageNames().addValueObserver(names => touchPage = names
        .zipWithIndex.find(_._1 == touchFX).map { case (_, idx) =>
        secondCursor.selectedPageIndex().set(idx)
        secondCursor
      })


      override val subModes: Seq[SubModeLayer] = Vector(
        new SliderBankMode[RemoteControl]("strips remote", page.c.getParameter, identity) {
          override val barMode: BarMode = BarMode.SINGLE

          j.stripBank.strips.forindex { case (strip, idx) =>
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

      override val modeBindings: Seq[Binding[_, _, _]] = Vector(
        SupBooleanB(j.left.light.isOn, m(() => device.hasPrevious.get(), page.hasPrevious)),
        SupBooleanB(j.right.light.isOn, m(() => device.hasNext.get(), page.hasNext)),
        HB(j.left.pressedAction, "scroll left", m(() => device.selectPrevious(), page.selectPrevious)),
        HB(j.right.pressedAction, "scroll right", m(() => device.selectNext(), page.selectNext)),
      )

      override def activate(): Unit = {
        val idx = page.c.selectedPageIndex().get()
        val pageNames = page.c.pageNames().get()
        if (idx >= 0 && pageNames(idx) == touchFX)
          if (page.hasPrevious())
            page.selectPrevious()
          else
            page.selectNext()
        super.activate()
      }
    }

    // Final assembly of all mode layers
    val top       = Coexist(SimpleModeLayer("-^-", modeBindings = Vector.empty))
    val bottom    = SimpleModeLayer("_|_", modeBindings = Vector.empty)
    val unmanaged = SimpleModeLayer("_x_", modeBindings = Vector.empty)
    new ModeDGraph(
      init = Vector(levelCycle, sceneLayer),
      play -> top,
      position -> Coexist(tempoLayer),
      sceneLayer -> top,
      bottom -> Coexist(globalQuant, shiftTransport, shiftMatrix, globalShift, shiftPages),
      bottom -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
      trackGroup -> Exclusive(solo, mute),
      clipMatrix -> top,
      bottom -> Exclusive(levelCycle, auxLayer, deviceLayer),
      bottom -> Coexist(auxGate),
      bottom -> Coexist(sceneLayer, superScene),
      bottom -> Coexist(unmanaged),
    )
  }
}
