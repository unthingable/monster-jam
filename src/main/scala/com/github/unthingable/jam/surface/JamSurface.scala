package com.github.unthingable.jam.surface

import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.callback.IntegerValueChangedCallback
import com.bitwig.extension.controller.api.{HardwareButton, OnOffHardwareLight, RelativeHardwareKnob}
import com.github.unthingable.framework.binding.{HB, ButtonEvt}
// import com.github.unthingable.jam.surface.JamControl.HbOps
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.framework.HasId
import com.github.unthingable.jam.surface.KeyMaster.JC
import com.github.unthingable.jam.surface.KeyMaster.RawButtonEvent


/* Surface model with all the controls, wired to MIDI */

class JamSurface(implicit ext: MonsterJamExt) extends Util {

  private def b(id: String) = {
    val info = ext.xmlMap.button(id)
    val button = JamControl.button(info)
    val led = JamControl.onOffLight(info)
    JamOnOffButton(id, button, led)
  }

  object Mod {
    object Shift extends HasButtonState, HasId, HasFakeButton {
      val id  = "SHIFT"
      val btn = FakeButton(id)
      val st  = ButtonStateSupplier(this, btn)

      btn.pressedAction.addBinding(JamControl.handle(id, RawButtonEvent.Press))
      btn.releasedAction.addBinding(JamControl.handle(id, RawButtonEvent.Release))

      override def toString: String = "BtnSHIFT"
    }

    var blink : Boolean = false // on 50% of the time
    var blink3: Boolean = false // on 75% of the time

    private def blinky(): Unit = {
      blink = true
      blink3 = true
      ext.host.scheduleTask(() => blinky(), 400)
      ext.host.scheduleTask(() => blink = false, 200)
      ext.host.scheduleTask(() => blink3 = false, 300)
    }
    ext.host.scheduleTask(() => blinky(), 100)
  }


  /* Left side */

  val song: JamOnOffButton = b("BtnArrange")

  val step = b("BtnStep")
  val pad = b("BtnPadMode")

  val clear = b("BtnClear")
  val duplicate = b("BtnDuplicate")
  val noteRepeat = b("BtnArpRepeat")

  val macroButton = b("BtnMacro")
  val level = b("BtnLevel")
  val aux = b("BtnAux")
  val control = b("BtnControl")
  val auto = b("BtnAuto")

  object encoder {
    val push: HardwareButton = JamControl.button(ext.xmlMap.button("PshBrowse", ext.xmlMap.masterElems))
    val touch: HardwareButton = JamControl.button(ext.xmlMap.button("CapBrowse", ext.xmlMap.masterElems))
    val turn: RelativeHardwareKnob = {
      val enc: MidiInfo = ext.xmlMap.wheel("EncBrowse", ext.xmlMap.masterElems)
      val knob: RelativeHardwareKnob = ext.hw.createRelativeHardwareKnob(enc.id)

      knob.setAdjustValueMatcher(ext.midiIn.createRelative2sComplementCCValueMatcher(
        enc.channel, enc.event.value, 127))
      knob.setStepSize(1 / 127.0)
      knob
    }
  }

  object dpad {
    private def b(idx: Int) = {
      val id = s"BtnDpad$idx"
      val info = ext.xmlMap.button(id)
      val button = JamControl.button(info)
      val led = JamControl.onOffLight(info)
      JamOnOffButton(id, button, led)
    }

    val up: JamOnOffButton = b(1)
    val down: JamOnOffButton = b(3)
    val left: JamOnOffButton = b(4)
    val right: JamOnOffButton = b(2)
  }

  /* Main section */

  val sceneButtons: Seq[JamRgbButton] = (1 to 8).map { idx =>
    val id = s"BtnScene${idx}"
    val btnInfo = ext.xmlMap.button(id)
    // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
    val btnLedInfo = ext.xmlMap.led(s"BtnScene${idx}IDX").copy(channel = btnInfo.channel)

    val button = JamControl.button(btnInfo)
    val led    = JamControl.rgbLight(btnLedInfo)

    button.setIndexInGroup(idx)

    JamRgbButton(id, button, led)
  }

  // matrix is indexed as row x col instead of col x row - weird but useful
  val matrix: Seq[Seq[JamRgbButton]] = (1 to 8).map { row =>
    ('A' to 'H').map { col =>
      val id = s"Btn$col$row"
      val btnInfo = ext.xmlMap.button(id, ext.xmlMap.matrixElems)
      val ledInfo = ext.xmlMap.led(s"Btn$col${row}IDX", ext.xmlMap.matrixElems)

      val button = JamControl.button(btnInfo)
      val led = JamControl.rgbLight(ledInfo)

      JamRgbButton(id, button, led)
    }
  }

  val groupButtons: Seq[JamRgbButton] = ('A' to 'H').map { idx =>
    val id = s"BtnGroup${idx}"
    val btnInfo = ext.xmlMap.button(id)
    // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
    val ledInfo = ext.xmlMap.led(s"BtnGroup${idx}IDX").copy(channel = btnInfo.channel)

    val button = JamControl.button(btnInfo)
    val led = JamControl.rgbLight(ledInfo)

    button.setIndexInGroup(idx)

    JamRgbButton(id, button, led)
  }

  // Touchstrips
  val stripBank = new StripBank()(ext)

  // Main level meters are special
  object levelMeter {
    val left = ext.xmlMap.knob("MetLevel1", ext.xmlMap.masterElems)
    val right = ext.xmlMap.knob("MetLevel2", ext.xmlMap.masterElems)

    def update(info: MidiInfo)(level: Int): Unit =
      ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, info.event.value, level)

    val uL: IntegerValueChangedCallback = update(left) _
    val uR: IntegerValueChangedCallback = update(right) _
  }

  /* Right side */

  val master = b("BtnMst")
  val group = b("BtnGrp")
  val in1 = b("BtnIn1")
  val cue = b("BtnCue")

  val browse = b("BtnBrowse")
  val perform = b("BtnPerform")
  val notes = b("BtnVariation")
  val lock = b("BtnLock")
  val tune = b("BtnTune")
  val swing = b("BtnSwing")
  val select = b("BtnSelect")

  // bottom
  val play = b("BtnPlay")
  val record = b("BtnRecord")
  val left = b("BtnArrowLeft")
  val right = b("BtnArrowRight")
  val tempo = b("BtnTempo")
  val grid = b("BtnGrid")
  val solo = b("BtnSolo")
  val mute = b("BtnMute")

  {
    // wire sysex
    import com.github.unthingable.jam.surface.BlackSysexMagic._

    ext.midiIn.setSysexCallback {
      case ShiftDownCommand =>
        // ext.events.eval(Mod.Shift.st.press.value)
        Mod.Shift.btn.pressedAction.invoke()
      case ShiftReleaseCommand =>
        // ext.events.eval(Mod.Shift.st.release.value)
        Mod.Shift.btn.releasedAction.invoke()
      case ReturnFromHostCommand =>
        ext.host.println("return from host")
        ext.hw.invalidateHardwareOutputState()
        ext.host.requestFlush()
      case x => "Unhandled sysex from controller: " + ext.host.println(x)
    }
  }

  // import Combo.JC
  val ShiftDup = JC(duplicate, Mod.Shift)
  val ShiftSolo = JC(solo, Mod.Shift)
}
