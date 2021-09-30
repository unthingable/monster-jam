package com.github.unthingable.jam.surface

import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.callback.IntegerValueChangedCallback
import com.bitwig.extension.controller.api.{BooleanValue, HardwareButton, OnOffHardwareLight, RelativeHardwareKnob}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.{HB, surface}


/* Surface model with all the controls, wired to MIDI */

trait HasButton {val button: FakeButton }

class JamSurface(implicit ext: MonsterJamExt) extends Util {

  private def b(id: String) = JamOnOffButton(ext.xmlMap.button(id))

  object Modifiers {
    var Shift: OnOffButton with HasButton = new OnOffButton with HasButton {
      override val button: FakeButton         = FakeButton()
      override val light : OnOffHardwareLight = ext.hw.createOnOffHardwareLight("shift_LED") // fake
      override val pressedAction: HB.HBS      = button.pressedAction
      override val releasedAction: HB.HBS     = button.releasedAction
      override val isPressed: () => Boolean   = () => button.isPressed
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
    val push: HardwareButton = JamButton(ext.xmlMap.button("PshBrowse", ext.xmlMap.masterElems)).button
    val touch: HardwareButton = JamButton(ext.xmlMap.button("CapBrowse", ext.xmlMap.masterElems)).button
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
    private def dpadInfo(idx: Int): MidiInfo = ext.xmlMap.button(s"BtnDpad$idx")

    private def button(idx: Int) = {
      val info = dpadInfo(idx)
      JamOnOffButton(info)
    }

    val up: JamOnOffButton = button(1)
    val down: JamOnOffButton = button(3)
    val left: JamOnOffButton = button(4)
    val right: JamOnOffButton = button(2)
  }

  /* Main section */

  val sceneButtons: Vector[JamRgbButton] = (1 to 8).map { idx =>
    val btn = ext.xmlMap.button(s"BtnScene${idx}")
    val btnLed = ext.xmlMap.led(s"BtnScene${idx}IDX")
    surface.JamRgbButton(
      infoB = btn,
      // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
      infoL = btnLed.copy(channel = btn.channel)
    )
  }.toVector.forindex(_.button.setIndexInGroup(_))

  val matrix: Vector[Vector[JamRgbButton]] = (1 to 8).map { row =>
    ('A' to 'H').map { col =>
      val btnInfo = ext.xmlMap.button(s"Btn$col$row", ext.xmlMap.matrixElems)
      val ledInfo = ext.xmlMap.led(s"Btn$col${row}IDX", ext.xmlMap.matrixElems)
      surface.JamRgbButton(btnInfo, ledInfo)
    }.toVector
  }.toVector

  val groupButtons: Vector[JamRgbButton] = ('A' to 'H').map { idx =>
    val btn = ext.xmlMap.button(s"BtnGroup${idx}")
    val btnLed = ext.xmlMap.led(s"BtnGroup${idx}IDX")
    surface.JamRgbButton(
      infoB = btn,
      // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
      infoL = btnLed.copy(channel = btn.channel)
    )
  }.toVector.forindex(_.button.setIndexInGroup(_))

  // Touchstrips
  val stripBank = StripBank()(ext)

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
        Modifiers.Shift.button.pressedAction.invoke()
      case ShiftReleaseCommand =>
        Modifiers.Shift.button.releasedAction.invoke()
      case ReturnFromHostCommand =>
        ext.host.println("return from host")
        ext.hw.invalidateHardwareOutputState()
        ext.host.requestFlush()
      case x => "Unhandled sysex from controller: " + ext.host.println(x)
    }
  }
}
