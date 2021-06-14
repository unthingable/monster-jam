package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.ColorValueChangedCallback
import com.bitwig.extension.controller.api._
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface._

import java.time.{Duration, Instant}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
Behavior definition for surface controls
 */

class Jam(implicit ext: MonsterJamExt) extends ModeLayerDSL {

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

  val sliderParams: ArrayBuffer[Parameter] = mutable.ArrayBuffer.from(
    j.stripBank.strips.indices.map(trackBank.getItemAt(_).volume())
  )
  var trackTrackColor: Boolean = true
  for (i <- j.stripBank.strips.indices) {
    val strip = j.stripBank.strips(i)
    val track = trackBank.getItemAt(i)
    sliderParams(i).markInterested()
    track.exists().markInterested()
    strip.slider.setBindingWithRange(sliderParams(i), 0, 1)

    val touchP = ext.host.createAction(() => {
      val current                    = sliderParams(i).get
      var startValue: Option[Double] = None
      strip.setOffsetCallback { v =>
        val offset = (v - startValue.getOrElse(v)) * 0.2
        sliderParams(i).set(current + offset)
        if (startValue.isEmpty) startValue = Some(v)
      }
    }, () => "shift")

    val touchR = ext.host.createAction(() => strip.clearOffsetCallback(), () => "shift")

    j.Modifiers.Shift.pressedAction.addBinding(action("shit-strips pressed", { () =>
      strip.slider.clearBindings()
      strip.button.pressedAction().setBinding(touchP)
      strip.button.releasedAction().setBinding(touchR)
      ()
    }))
    j.Modifiers.Shift.releasedAction.addBinding(action("shift-strips released", { () =>
      strip.clearOffsetCallback()
      strip.button.pressedAction().clearBindings()
      strip.button.releasedAction().clearBindings()
      strip.slider.setBinding(sliderParams(i))
      ()
    }))

    track.exists().addValueObserver(j.stripBank.setActive(i, _))
    sliderParams(i).value().markInterested()
    sliderParams(i).value().addValueObserver(128, j.stripBank.setValue(i, _)) // move fader dot

    track.addVuMeterObserver(128, -1, true, strip.update)

    track.color().markInterested()
    track.color().addValueObserver((r, g, b) =>
      if (trackTrackColor) j.stripBank.setColor(i, NIColorUtil.convertColor(r, g, b)))

    //ValObserverB[ColorValueChangedCallback, JamTouchStrip](
    //  track.color(),
    //  (r, g, b) => j.stripBank.setColor(i, NIColorUtil.convertColor(r, g, b)),
    //  strip)
  }

  //def stripLayer(name: String, button: JamOnOffButton) = new ModeButtonLayer(name, button) {
  //  override val modeBindings: Seq[Binding[_, _, _]] = Seq.empty
  //
  //
  //}

  val levelLayer = new ModeButtonLayer("strips level", j.level, gateMode = GateMode.OneWay) {
    override val modeBindings: Seq[Binding[_, _, _]] = Seq.empty
      //j.stripBank.strips.zipWithIndex.map { case (strip, idx) =>
      //  val track = trackBank.getItemAt(idx)
      //  ValObserverB[ColorValueChangedCallback, JamTouchStrip](
      //    track.color(),
      //    (r, g, b) => j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)),
      //    strip)
      //}

    override def activate(): Unit = {
      super.activate()
      trackTrackColor = true
      ext.host.println("+++ activated")
    }

    override def deactivate(): Unit = {
      trackTrackColor = false
      super.deactivate()
    }
  }

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
      j.stripBank.strips.forindex { case (_, idx) =>
        j.stripBank.setColor(idx, JamColorState.toColorIndex(toColor(java.awt.Color.WHITE)))
      }
    }
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
        (j.Modifiers.Shift.isPressed, forward) match {
          case (false, true)  => target.scrollPageForwards()
          case (false, false) => target.scrollPageBackwards()
          case (true, true)   => target.scrollForwards()
          case (true, false)  => target.scrollBackwards()
        }, () => s"scroll_$forward")
    }

    j.dpad.left.button.pressedAction.setBinding(scroll(false, trackBank))
    j.dpad.right.button.pressedAction.setBinding(scroll(true, trackBank))
    j.dpad.up.button.pressedAction.setBinding(scroll(false, sceneBank))
    j.dpad.down.button.pressedAction.setBinding(scroll(true, sceneBank))

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
          HB(btn.button.pressedAction, s"scene $i press", () => handlePress(scene)))
      }

      private def handlePress(scene: Scene): Unit = {
        if (GlobalMode.Clear.isOn) scene.deleteObject()
        else if (GlobalMode.Duplicate.isOn) scene.nextSceneInsertionPoint().copySlotsOrScenes(scene)
             else scene.launchAction().invoke()
      }
    }

    val navLayer = SimpleModeLayer("position",
      Seq(HB(j.encoder.turn, ext.host.createRelativeHardwareControlStepTarget(
        ext.transport.fastForwardAction(),
        ext.transport.rewindAction()))))

    val tempoLayer = ModeButtonLayer("tempo",
      j.tempo,
      Seq(
        HB(j.encoder.turn, ext.host.createRelativeHardwareControlStepTarget(
          action("inc tempo", () => ext.transport.increaseTempo(1, 647)),
          action("dec tempo", () => ext.transport.increaseTempo(-1, 647))))))

    val play = new SimpleModeLayer("play") {
      ext.transport.isPlaying.markInterested()

      val playPressAction: HardwareActionBindable = action(s"$name play pressed", () => {
        val isPlaying = ext.transport.isPlaying
        val t         = ext.transport
        (isPlaying.get(), j.Modifiers.Shift.isPressed) match {
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
        HB(j.play.button.pressedAction, playPressAction, tracked = false),
        SupBooleanB(j.play.light.isOn, ext.transport.isPlaying),
      )
    }

    val shiftLoadActions: LoadActions = LoadActions(
      activate = j.Modifiers.Shift.pressedAction,
      deactivate = j.Modifiers.Shift.releasedAction
    )

    val loop = new ModeActionLayer("loop", loadActions = shiftLoadActions) {
      val loop = ext.transport.isArrangerLoopEnabled
      loop.markInterested()

      override val modeBindings = Seq(
        HB(j.right.button.pressedAction(), "loop pressed", () => loop.toggle()),
        SupBooleanB(j.right.light.isOn, loop)
      )
    }

    val globalQuant = new ModeActionLayer("globalQuant",
      loadActions = LoadActions(
        activate = j.grid.button.pressedAction,
        deactivate = j.grid.button.releasedAction
      )
    ) {
      val quant = ext.transport.defaultLaunchQuantization()
      quant.markInterested()
      val enumValues = Vector("8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16")
      override val modeBindings = j.sceneButtons.indices.flatMap { idx =>
        val sceneButton = j.sceneButtons(idx)

        Seq(
          SupColorB(sceneButton.light, () =>
            if (quant.get() == enumValues(idx)) Color.whiteColor() else Color.blackColor()),
          HB(sceneButton.button.pressedAction(), action(s"grid $idx", () => {
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
          HB(gButton.button.pressedAction(), s"group $idx pressed: $name", () => propValue.toggle()),
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
          HB(btn.button.pressedAction(), s"group $idx pressed: select in mixer", () => handlePress(track))
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
            HB(btn.button.pressedAction(), s"clipPress $row:$col", () => handleClipPress(clip, clips)),
            HB(btn.button.releasedAction(), s"clipRelease $row:$col", () => handleClipRelease(clip, clips)),
          )
        }
      } :+ HB(GlobalMode.Duplicate.deactivateAction, "dup clips: clear source", () => {
        //ext.host.println("boom")
        source = None
      }, tracked = false, managed = false)

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
    val shiftMatrix = new ModeActionLayer("shiftMatrix", loadActions = shiftLoadActions) {
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
        button.button.isPressed.markInterested()
        Seq(
          HB(button.button.pressedAction(), s"shift-$idx matrix pressed", action),
          SupColorStateB(
            button.light, () => JamColorState(
              color * 4,
              brightness = if (button.button.isPressed.get()) 2 else 0),
            JamColorState.empty)
        )
      }
    }

    val globalShift = new ModeActionLayer("globalShift", loadActions = shiftLoadActions) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] = Seq(
        HB(j.duplicate.button.pressedAction, "shift dup clip content", () => clip.duplicateContent())
      )
    }

    val top    = Coexist(SimpleModeLayer("-^-", modeBindings = Seq.empty))
    val bottom = SimpleModeLayer("_|_", modeBindings = Seq.empty)
    new ModeDGraph(
      init = Seq(levelLayer),
      play -> top,
      navLayer -> Coexist(tempoLayer),
      sceneLayer -> top,
      bottom -> Coexist(globalQuant, loop, shiftMatrix, globalShift),
      bottom -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
      trackGroup -> Exclusive(solo, mute),
      clipMatrix -> top,
      bottom -> Exclusive(levelLayer, auxLayer),
    )
  }

  // for now
  ext.host.scheduleTask(() => ext.hw.invalidateHardwareOutputState(), 200)
}
