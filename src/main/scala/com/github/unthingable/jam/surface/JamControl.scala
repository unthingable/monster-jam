package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.controller.api._
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.surface.BlackSysexMagic.{BarMode, createCommand}

import scala.collection.mutable

// Create hardware controls and wire them to MIDI
class JamButton(ext: MonsterJamExt, info: MidiInfo) {
  val button: HardwareButton = JamControl.button(ext, info)
}

object JamControl {

  def button(ext: MonsterJamExt, info: MidiInfo): HardwareButton = {
    val button = ext.hw.createHardwareButton(info.id)
    val (on, off) = info.event match {
      case CC(cc) => (
        ext.midiIn.createCCActionMatcher(info.channel, cc, 127),
        ext.midiIn.createCCActionMatcher(info.channel, cc, 0)
      )
      case Note(note) => (
        ext.midiIn.createNoteOnActionMatcher(info.channel, note),
        ext.midiIn.createNoteOffActionMatcher(info.channel, note)
      )
    }
    button.pressedAction.setActionMatcher(on)
    button.releasedAction.setActionMatcher(off)

    def handlePressed(id: String): Runnable = () => ext.host.println(s"$id pressed")

    def handleReleased(id: String): Runnable = () => ext.host.println(s"$id released")

    button.pressedAction.setBinding(ext.host.createAction(handlePressed(info.id), () => "Handle button pressed"))
    button.releasedAction.setBinding(ext.host.createAction(handleReleased(info.id), () => "Handle button released"))
    button
  }

  def slider(ext: MonsterJamExt, info: MidiInfo): HardwareSlider = {
    val slider = ext.hw.createHardwareSlider(info.id)
    // assume it's always CC
    slider.setAdjustValueMatcher(ext.midiIn.createAbsoluteCCValueMatcher(info.channel, info.event.value))
    slider
  }

  case class JamColorState(color: Int) extends InternalHardwareLightState {
    override def getVisualState: HardwareLightVisualState = null
  }

  def rgbLight(ext: MonsterJamExt, info: MidiInfo): MultiStateHardwareLight = {
    val light = ext.hw.createMultiStateHardwareLight(info.id + "_LED")
    light.setColorToStateFunction(toState)
    light.state().onUpdateHardware { state: JamColorState =>
      info.event match {
        case CC(cc) =>
          //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${state.color}")
          ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, cc, state.color)
        case Note(note) =>
          //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${state.color}")
          ext.midiOut.sendMidi(ShortMidiMessage.NOTE_ON + info.channel, note, state.color)
      }
    }
    light.state().setValue(JamColorState(0))
    light
  }

  def toState(color: Color): InternalHardwareLightState = JamColorState(toColorIndex(color))

  def toColorIndex(color: Color): Int =
    NIColorUtil.convertColor(color.getRed.toFloat, color.getGreen.toFloat, color.getBlue.toFloat)

  def onOffLight(ext: MonsterJamExt, info: MidiInfo): OnOffHardwareLight = {
    val led = ext.hw.createOnOffHardwareLight(info.id + "_LED")
    led.onUpdateHardware { () =>
      info.event match {
        case CC(cc) =>
          ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${led.isOn.currentValue()}")
          ext.midiOut.sendMidi(
            ShortMidiMessage.CONTROL_CHANGE + info.channel,
            cc,
            if (led.isOn.currentValue) 127 else 0)
        case Note(note) =>
          ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${led.isOn.currentValue()}")
          ext.midiOut.sendMidi(
            ShortMidiMessage.NOTE_ON + info.channel,
            note,
            if (led.isOn.currentValue) 127 else 0)
      }
    }
    led.isOn.setValue(false)
    led
  }
}

abstract class JamLitButton[L <: HardwareLight](ext: MonsterJamExt, infoB: MidiInfo, infoL: MidiInfo)
  extends JamButton(ext, infoB) {
  val light: L
}

case class JamRgbButton(ext: MonsterJamExt, infoB: MidiInfo, infoL: MidiInfo)
  extends JamLitButton[MultiStateHardwareLight](ext, infoB, infoL) {
  val light: MultiStateHardwareLight = JamControl.rgbLight(ext, infoL)
}

case class JamOnOffButton(ext: MonsterJamExt, info: MidiInfo)
  extends JamLitButton[OnOffHardwareLight](ext, info, info) {
  val light: OnOffHardwareLight = JamControl.onOffLight(ext, info)
}

case class JamVUStrip(ext: MonsterJamExt, touch: MidiInfo, slide: MidiInfo) {
  val button: HardwareButton = JamControl.button(ext, touch)
  val slider: HardwareSlider = JamControl.slider(ext, slide)
  // leds handled by sysex magic

  //val light: MultiStateHardwareLight = JamControl.rgbLight(ext, led)
  //
  def update(value: Int): Unit = {
    //ext.host.println(s"updating slider ${slide} to $level")
    ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + slide.channel, slide.event.value, value)
  }
}

case class StripBank(ext: MonsterJamExt) {
  val strips: Vector[JamVUStrip] = ('A' to 'H').map { idx =>
    JamVUStrip(ext,
      touch = ext.xmlMap.button(s"CapTst$idx", ext.xmlMap.touchElems),
      slide = ext.xmlMap.knob(s"Tst$idx", ext.xmlMap.touchElems))
  }.toVector

  var barMode: BarMode = BarMode.DUAL
  private val colors: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val values: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)

  def setColor(idx: Int, color: Int): Unit = {
    colors.update(idx, color)
    flushColors()
  }
  def setValue(idx: Int, value: Int): Unit = {
    values.update(idx, value)
    flushValues()
  }

  def flushColors(): Unit =
    ext.midiOut.sendSysex(createCommand("05", colors.map(n => f"${barMode.v}${n}%02x").mkString))
  def flushValues(): Unit = {
    //clear()
    ext.midiOut.sendSysex(createCommand("04", values.map(n => f"${n}%02x").mkString))
  }

  def clear(): Unit = ext.midiOut.sendSysex(BlackSysexMagic.zeroStrips)
}
