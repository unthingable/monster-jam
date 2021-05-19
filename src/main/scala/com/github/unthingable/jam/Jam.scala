package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.{BooleanValueChangedCallback, ColorValueChangedCallback}
import com.bitwig.extension.controller.api.{BooleanValue, Channel, HardwareAction, HardwareActionBindable, HardwareActionBinding, Parameter, Scene, SceneBank, Scrollable, SettableBooleanValue, SettableColorValue, Track, TrackBank}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.{JamColor, JamColorState, JamOnOffButton, JamRgbButton, JamSurface, NIColorUtil}

import java.util.function.Supplier



class Jam(implicit ext: MonsterJamExt) extends ModeLayerDSL {
  // wire stuff
  val j = new JamSurface(ext)

  // wire buttons

    //val trackBank: TrackBank = ext.host.createMainTrackBank(8,8,8)
  val trackBank = ext.trackBank
  trackBank.followCursorTrack(ext.cursorTrack)
    val sceneBank: SceneBank = trackBank.sceneBank()
    val masterTrack = ext.host.createMasterTrack(8)

    sceneBank.canScrollForwards.markInterested()
    sceneBank.canScrollBackwards.markInterested()
    sceneBank.itemCount().markInterested()

    trackBank.cursorIndex().markInterested()
    trackBank.setSkipDisabledItems(true)
    //sceneBank.setIndication(true)

    // wire scene buttons
  val sceneLayer = new SimpleModeLayer("scene") {
    override val modeBindings: Seq[Binding[_, _, _]] =
      j.sceneButtons.indices.flatMap { i =>
        val btn: JamRgbButton = j.sceneButtons(i)
        val scene: Scene = sceneBank.getScene(i)
        scene.color.markInterested()
        scene.exists.markInterested()
        Seq(
          SupColorB(btn.light, scene.color()),
          HB(btn.button.pressedAction(), scene.launchAction()))
      }
  }

    // wire track group buttons
    for (col <- j.groupButtons.indices) {
      val btn = j.groupButtons(col)
      val track = trackBank.getItemAt(col)
      track.clipLauncherSlotBank().setIndication(true)

      track.color().markInterested()
      track.exists().markInterested()
      //btn.light.setColorSupplier(track.color())
      //
      //track.addIsSelectedInEditorObserver { v =>
      //  btn.jamLight.updatedColorState = btn.jamLight.updatedColorState.copy(brightness = if (v) 2 else 0)
      //}
      //
      //track.playingNotes().markInterested()
      //track.playingNotes().addValueObserver { notes =>
      //  btn.jamLight.sendColor(btn.jamLight.updatedColorState.copy(brightness = if (notes.nonEmpty) 2 else btn.jamLight.updatedColorState.brightness))
      //}

      // wire clips to matrix
      val clips = track.clipLauncherSlotBank()
      for (row <- 0 until 8) {
        val btn = j.matrix(row)(col)
        val clip = clips.getItemAt(row)
        clip.color().markInterested()
        clips.exists().markInterested()
        btn.light.setColorSupplier(clip.color())
      }


    // wire strips
    j.stripBank.flushColors()
    for (i <- j.stripBank.strips.indices) {
      val strip = j.stripBank.strips(i)
      val track = trackBank.getItemAt(i)
      val sliderParam: Parameter = track.volume()
      sliderParam.markInterested()
      track.exists().markInterested()
      strip.slider.setBindingWithRange(sliderParam, 0, 1)

      val touchP = ext.host.createAction(() => {
        val current = sliderParam.get
        var startValue: Option[Double] = None
        strip.setOffsetCallback { v =>
          val offset = (v - startValue.getOrElse(v)) * 0.2
          sliderParam.set(current + offset)
          if (startValue.isEmpty) startValue = Some(v)
        }
      }, () => "shift")

      val touchR = ext.host.createAction(() => strip.clearOffsetCallback(), () => "shift")

      j.Modifiers.Shift.pressedAction.addBinding { () =>
        strip.slider.clearBindings()
        strip.button.pressedAction().setBinding(touchP)
        strip.button.releasedAction().setBinding(touchR)
      }
      j.Modifiers.Shift.releasedAction.addBinding { () =>
        strip.clearOffsetCallback()
        strip.button.pressedAction().clearBindings()
        strip.button.releasedAction().clearBindings()
        strip.slider.setBinding(sliderParam)
      }

      track.exists().addValueObserver(j.stripBank.setActive(i, _))
      sliderParam.value().markInterested()
      sliderParam.value().addValueObserver(128, j.stripBank.setValue(i, _)) // move fader dot

      track.addVuMeterObserver(128, -1, true, strip.update)

      track.color().markInterested()
      track.color().addValueObserver((r,g,b) => j.stripBank.setColor(i, NIColorUtil.convertColor(r,g,b)))
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
  }

  {
    // preloaded
    val navLayer = new SimpleModeLayer("position",
      Seq(HB(j.encoder.turn, ext.host.createRelativeHardwareControlStepTarget(
        ext.transport.fastForwardAction(),
        ext.transport.rewindAction()))))

    //val stack = new LayerStack(navLayer)

    // preloaded
    val tempoLayer = new ModeButtonLayer("tempo",
      j.tempo,
      Seq(
        HB(j.encoder.turn, ext.host.createRelativeHardwareControlStepTarget(
          action("inc tempo", () => ext.transport.increaseTempo(1, 647)),
          action("dec tempo", () => ext.transport.increaseTempo(-1, 647))))))

    //stack.load(tempoLayer)

    val play = new SimpleModeLayer("play") {
      ext.transport.isPlaying.markInterested()

      /*
      play button light <- is playing
      play press -> start/stop
      play shift-press -> stop/rewind
       */
      val playPressAction: HardwareActionBindable = action(s"$name play pressed", () => {
        val isPlaying = ext.transport.isPlaying.getAsBoolean
        val t = ext.transport
        (isPlaying, j.Modifiers.Shift.isPressed) match {
          // needs work
            // just play
          case (true, false) => t.play()
            // restart
          case (true, true) => restart()
            // resume
          case (false, false) => t.continuePlayback()
          case (false, true) => t.restart()
        }
      })

      def restart(): Unit = {
        val h = ext.host
        h.scheduleTask(() => {
          ext.transport.stop()
          h.scheduleTask(() => {
            ext.transport.stop()
            h.scheduleTask(() => ext.transport.play(), 1)
          }, 1)
        }, 1)
      }

      override val modeBindings = Seq(
        HB(j.play.button.pressedAction, playPressAction),
        asB(j.play.light.isOn -> ext.transport.isPlaying),
      )
    }

    val loop = new ModeActionLayer("loop", loadActions = LoadActions(
      activate = j.Modifiers.Shift.pressedAction,
      deactivate = j.Modifiers.Shift.releasedAction
    )) {
      val loop = ext.transport.isArrangerLoopEnabled
      loop.markInterested()

      override val modeBindings = Seq(
        HB(j.right.button.pressedAction(), () => loop.toggle()),
        SupBooleanB(j.right.light.isOn, loop)
      )
    }

    val performGrid = new ModeActionLayer("performGrid",
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
    ): ModeButtonLayer = new ModeButtonLayer(name, modeButton) {
      override val modeBindings: Seq[Binding[_, _, _]] = group.indices.flatMap { idx =>
        val track = trackBank.getItemAt(idx)
        val propValue = prop(track)
        val existsValue = track.exists()
        propValue.markInterested()
        existsValue.markInterested()
        val gButton = group(idx)

        Seq(
          HB(gButton.button.pressedAction(), () => propValue.toggle()),
          SupColorStateB(gButton.light, () => {
            (existsValue.get(), propValue.get()) match {
              case (false, _) => JamColorState.empty
              case (_, false) => JamColorState(color * 4, 0)
              case (_, true) => JamColorState(color * 4, 3)
            }
          }, JamColorState.empty)
        )
      }
    }

    val solo = buttonGroupChannelMode("solo", j.solo, j.groupButtons, _.solo(), JAMColorBase.YELLOW)
    val mute = buttonGroupChannelMode("mute", j.mute, j.groupButtons, _.mute(), JAMColorBase.ORANGE)
    val trackGroup = new SimpleModeLayer("trackGroup") {
      override val modeBindings: Seq[Binding[_, _, _]] = j.groupButtons.indices flatMap { idx =>
        val btn = j.groupButtons(idx)

        val track = trackBank.getItemAt(idx)
        val color = track.color()
        val cursorIndex = trackBank.cursorIndex()
        val playingNotes = track.playingNotes()

        color.markInterested()
        playingNotes.markInterested()
        cursorIndex.markInterested()
        track.exists().markInterested()

        Seq(
          SupColorStateB(btn.light, () => JamColorState(
            JamColorState.toColorIndex(color.get()),
            brightness = (playingNotes.get().length > 0, cursorIndex.get() == idx) match {
              case (_, true) => 3
              case (true, _) => 2
              case (false, _) => 0
            }
          ), JamColorState.empty),
          //HB(btn.button.pressedAction(), () => trackBank.cursorIndex().set(idx))
          HB(btn.button.pressedAction(), () => track.selectInMixer())
        )
      }
    }

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
            SupColorStateB(btn.light, () => JamColorState(
              JamColorState.toColorIndex(clip.color().get()),
              brightness = {
                if (clip.isPlaying.get())
                  if (track.isQueuedForStop.get()) 3 - j.Modifiers.blink
                  else 3
                else if (clip.isPlaybackQueued.get()) j.Modifiers.blink
                else 0
              }
            ), JamColorState.empty),
            HB(btn.button.pressedAction(), () =>
              if (clip.isPlaying.get()) clips.stop()
              else clip.launch()
            ),
          )
        }
      }
    }

    val top = Coexist(new SimpleModeLayer("-^-"))
    val bottom = new SimpleModeLayer("_|_")
    new ModeDGraph(
      play -> top,
      navLayer -> Coexist(tempoLayer),
      sceneLayer -> top,
      bottom -> Coexist(performGrid, loop),
      trackGroup -> Exclusive(solo, mute),
      clipMatrix -> top,
    )
  }

  // for now
  ext.host.scheduleTask(() => ext.hw.invalidateHardwareOutputState(), 200)
}
