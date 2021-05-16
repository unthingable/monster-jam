package com.github.unthingable.jam

import com.bitwig.extension.callback.BooleanValueChangedCallback
import com.bitwig.extension.controller.api.{BooleanValue, HardwareAction, HardwareActionBindable, HardwareActionBinding, Scene, SceneBank, Scrollable, SettableBooleanValue, TrackBank}
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.surface.{JamColor, JamOnOffButton, JamRgbButton, JamSurface, NIColorUtil}



class Jam(implicit ext: MonsterJamExt) extends ModeLayerDSL {
  // wire stuff
  val j = new JamSurface(ext)

  // wire buttons
  {
    val trackBank: TrackBank = ext.host.createMainTrackBank(8,8,8)
    val sceneBank: SceneBank = trackBank.sceneBank()
    val masterTrack = ext.host.createMasterTrack(8)

    sceneBank.canScrollForwards.markInterested()
    sceneBank.canScrollBackwards.markInterested()
    sceneBank.itemCount().markInterested()

    trackBank.cursorIndex().markInterested()
    trackBank.setSkipDisabledItems(true)
    //sceneBank.setIndication(true)

    // wire scene buttons
    for (i <- j.sceneButtons.indices) {
      val btn: JamRgbButton = j.sceneButtons(i)
      val scene: Scene = sceneBank.getScene(i)
      scene.color.markInterested()
      scene.exists.markInterested()
      btn.light.setColorSupplier(scene.color())
      // incomplete
      btn.button.pressedAction().addBinding(scene.launchAction())
    }

    // wire track group buttons
    for (col <- j.groupButtons.indices) {
      val btn = j.groupButtons(col)
      val track = trackBank.getItemAt(col)
      track.clipLauncherSlotBank().setIndication(true)

      track.color().markInterested()
      track.exists().markInterested()
      btn.light.setColorSupplier(track.color())

      track.addIsSelectedInEditorObserver { v =>
        btn.jamLight.updatedColorState = btn.jamLight.updatedColorState.copy(brightness = if (v) 2 else 0)
      }

      track.playingNotes().markInterested()
      track.playingNotes().addValueObserver { notes =>
        btn.jamLight.sendColor(btn.jamLight.updatedColorState.copy(brightness = if (notes.nonEmpty) 2 else btn.jamLight.updatedColorState.brightness))
      }

      // wire clips to matrix
      val clips = track.clipLauncherSlotBank()
      for (row <- 0 until 8) {
        val btn = j.matrix(row)(col)
        val clip = clips.getItemAt(row)
        clip.color().markInterested()
        clips.exists().markInterested()
        btn.light.setColorSupplier(clip.color())
      }
    }

    // wire strips
    j.stripBank.flushColors()
    for (i <- j.stripBank.strips.indices) {
      val strip = j.stripBank.strips(i)
      val track = trackBank.getItemAt(i)
      track.volume().markInterested()
      track.exists().markInterested()
      strip.slider.setBindingWithRange(track.volume(), 0, 1)

      val touchP = ext.host.createAction(() => {
        val current = track.volume().get
        var startValue: Option[Double] = None
        strip.setOffsetCallback { v =>
          val offset = (v - startValue.getOrElse(v)) * 0.2
          track.volume().set(current + offset)
          if (startValue.isEmpty) startValue = Some(v)
        }
      }, () => "shift")

      val touchR = ext.host.createAction(() => strip.clearOffsetCallback(), () => "shift")

      j.Modifiers.shiftPressed.addOne { () =>
        strip.slider.clearBindings()
        strip.button.pressedAction().setBinding(touchP)
        strip.button.releasedAction().setBinding(touchR)
      }
      j.Modifiers.shiftReleased.addOne { () =>
        strip.clearOffsetCallback()
        strip.button.pressedAction().clearBindings()
        strip.button.releasedAction().clearBindings()
        strip.slider.setBinding(track.volume())
      }

      track.exists().addValueObserver(j.stripBank.setActive(i, _))
      track.volume().value().markInterested()
      track.volume().value().addValueObserver(128, j.stripBank.setValue(i, _)) // move fader dot

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
          (j.Modifiers.Shift, forward) match {
            case (false, true) => target.scrollPageForwards()
            case (false, false) => target.scrollPageBackwards()
            case (true, true) => target.scrollForwards()
            case (true, false) => target.scrollBackwards()
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

  // layer experiment
  {
    //val enc: MidiInfo = ext.xmlMap.wheel("EncBrowse", ext.xmlMap.masterElems)
    //val knob: RelativeHardwareKnob = ext.hw.createRelativeHardwareKnob(enc.id)
    //
    //knob.setAdjustValueMatcher(ext.midiIn.createRelative2sComplementCCValueMatcher(
    //  enc.channel, enc.event.value, 127))
    //knob.setStepSize(1 / 127.0)


    // preloaded
    val navLayer = new ModeLayer("position", Map(j.encoder.turn -> ext.host.createRelativeHardwareControlStepTarget(
      ext.transport.fastForwardAction(),
      ext.transport.rewindAction())))

    val stack = new LayerStack(navLayer)

    val swing = j.swing
    // preloaded
    val swingLayer = new ModeLayer("swing",
      Map(j.encoder.turn -> ext.host.createRelativeHardwareControlStepTarget(
      ext.binding(() => ext.transport.increaseTempo(1,647), "inc tempo"),
      ext.binding(() => ext.transport.increaseTempo(-1,647), "dec tempo"))),
      InBindings(
        activate = Seq(swing.button.pressedAction),
        deactivate = Seq(swing.button.releasedAction)
      )
    )

    stack.load(swingLayer)

    //swing.button.pressedAction().addBinding(ext.binding(() => LayerStack.push(swingLayer), "push swing"))
    //val b = swing.button.releasedAction().addBinding(ext.binding(() => stack.pop(swingLayer), "pop swing"))
    //b.removeBinding()
  }
}
