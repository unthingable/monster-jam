package com.github.unthingable.jam.surface

import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.callback.IntegerValueChangedCallback
import com.bitwig.extension.controller.api.{HardwareButton, RelativeHardwareKnob}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface

// Surface model All the controls, wired to MIDI
class JamSurface(ext: MonsterJamExt) extends Util {
  //val ext: MonsterJamExt

  private def b(id: String) = JamOnOffButton(ext, ext.xmlMap.button(id))

  object Modifiers {
    var Shift: Boolean = false
  }

  // Left side
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
    val push: HardwareButton = JamButton(ext, ext.xmlMap.button("PshBrowse", ext.xmlMap.masterElems)).button
    val touch: HardwareButton = JamButton(ext, ext.xmlMap.button("CapBrowse", ext.xmlMap.masterElems)).button
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
      JamOnOffButton(ext, info)
    }

    val up: JamOnOffButton = button(1)
    val down: JamOnOffButton = button(3)
    val left: JamOnOffButton = button(4)
    val right: JamOnOffButton = button(2)
  }

  // Meat side
  val sceneButtons: Vector[JamRgbButton] = (1 to 8).map { idx =>
    val btn = ext.xmlMap.button(s"BtnScene${idx}")
    val btnLed = ext.xmlMap.led(s"BtnScene${idx}IDX")
    surface.JamRgbButton(ext,
      infoB = btn,
      // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
      infoL = btnLed.copy(channel = btn.channel)
    )
  }.toVector.forIndex(_.button.setIndexInGroup(_))

  val matrix: Vector[Vector[JamRgbButton]] = (1 to 8).map { row =>
    ('A' to 'H').map { col =>
      val btnInfo = ext.xmlMap.button(s"Btn$col$row", ext.xmlMap.matrixElems)
      val ledInfo = ext.xmlMap.led(s"Btn$col${row}IDX", ext.xmlMap.matrixElems)
      surface.JamRgbButton(ext, btnInfo, ledInfo)
    }.toVector
  }.toVector

  val groupButtons: Vector[JamRgbButton] = ('A' to 'H').map { idx =>
    val btn = ext.xmlMap.button(s"BtnGroup${idx}")
    val btnLed = ext.xmlMap.led(s"BtnGroup${idx}IDX")
    surface.JamRgbButton(ext,
      infoB = btn,
      // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
      infoL = btnLed.copy(channel = btn.channel)
    )
  }.toVector.forIndex(_.button.setIndexInGroup(_))

  // Touchstrips
  val stripBank = StripBank(ext)

  object levelMeter {
    val left = ext.xmlMap.knob("MetLevel1", ext.xmlMap.masterElems)
    val right = ext.xmlMap.knob("MetLevel2", ext.xmlMap.masterElems)

    def update(info: MidiInfo)(level: Int): Unit =
      ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, info.event.value, level)

    val uL: IntegerValueChangedCallback = update(left) _
    val uR: IntegerValueChangedCallback = update(right) _
  }

  // Right side
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

  val shift = {
    // TODO
    val button = ext.hw.createHardwareButton("SHIFT")
    //ext.midiIn.createActionMatcher()
    //val (on, off) = info.event match {
    //  case CC(cc) => (
    //    ext.midiIn.createCCActionMatcher(info.channel, cc, 127),
    //    ext.midiIn.createCCActionMatcher(info.channel, cc, 0)
    //  )
    //  case Note(note) => (
    //    ext.midiIn.createNoteOnActionMatcher(info.channel, note),
    //    ext.midiIn.createNoteOffActionMatcher(info.channel, note)
    //  )
    //}
    //button.pressedAction.setActionMatcher(on)
    //button.releasedAction.setActionMatcher(off)

    //def handlePressed(id: String): Runnable = () => ext.host.println(s"$id pressed")
    //
    //def handleReleased(id: String): Runnable = () => ext.host.println(s"$id released")
    //
    //button.pressedAction.setBinding(ext.host.createAction(handlePressed("SHIFT"), () => "Handle button pressed"))
    //button.releasedAction.setBinding(ext.host.createAction(handleReleased("SHIFT"), () => "Handle button released"))

    // wire sysex
    import com.github.unthingable.jam.surface.BlackSysexMagic._

    ext.midiIn.setSysexCallback {
      case ShiftDownCommand =>
        Modifiers.Shift = true
        ext.host.println("shift pressed")
      case ShiftReleaseCommand =>
        Modifiers.Shift = false
        ext.host.println("shift released")
      case ReturnFromHostCommand =>
        ext.host.println("return from host")
        ext.hw.invalidateHardwareOutputState()
        ext.host.requestFlush()
      case x => ext.host.println(x)
    }

    button
  }
}
