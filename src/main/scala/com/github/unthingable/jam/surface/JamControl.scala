package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.controller.api._
import com.github.unthingable.jam.PolyAction
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface.BlackSysexMagic.{BarMode, createCommand}

import scala.collection.mutable

// Create hardware controls and wire them to MIDI

sealed trait JamControl

trait Button extends JamControl { val button: HardwareButton }
trait Light[L <: HardwareLight] { val light: L }
trait RgbLight extends Light[MultiStateHardwareLight]
trait OnOffLight extends Light[OnOffHardwareLight]

case class JamButton(ext: MonsterJamExt, info: MidiInfo) extends Button {
  val button: HardwareButton = ext.hw.createHardwareButton(info.id)

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
  //button.pressedAction.setBinding(ext.host.createAction(handlePressed(info.id), () => "Handle button pressed"))
  //button.releasedAction.setBinding(ext.host.createAction(handleReleased(info.id), () => "Handle button released"))
  //
  //def handlePressed(id: String): Runnable = () => ext.host.println(s"$id pressed")
  //
  //def handleReleased(id: String): Runnable = () => ext.host.println(s"$id released")
}


case class JamRgbLight(ext: MonsterJamExt, info: MidiInfo) extends RgbLight {
  val light: MultiStateHardwareLight = ext.hw.createMultiStateHardwareLight(info.id + "_LED")
  var updatedColorState: JamColorState = JamColorState.empty

  light.setColorToStateFunction(toState)
  light.state().onUpdateHardware { state: JamColorState =>
    updatedColorState = state
    sendColor(state)
  }
  light.state().setValue(JamColorState.empty)

  case class JamColorState(color: Int, brightness: Int) extends InternalHardwareLightState {
    override def getVisualState: HardwareLightVisualState = null
    val value: Int = color + brightness
  }
  object JamColorState {
    val empty: JamColorState = JamColorState(0, 0)
  }

  def sendColor(color: JamColorState): Unit = {
    info.event match {
      case CC(cc) =>
        //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${state.color}")
        ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, cc, color.value)
      case Note(note) =>
        //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${state.color}")
        ext.midiOut.sendMidi(ShortMidiMessage.NOTE_ON + info.channel, note, color.value)
    }
  }

  def toState(color: Color): InternalHardwareLightState = JamColorState(toColorIndex(color), updatedColorState.brightness)

  def toColorIndex(color: Color): Int =
    NIColorUtil.convertColor(color.getRed.toFloat, color.getGreen.toFloat, color.getBlue.toFloat)
}

case class JamOnOffLight(ext: MonsterJamExt, info: MidiInfo) {
  val light: OnOffHardwareLight = ext.hw.createOnOffHardwareLight(info.id + "_LED")
  light.onUpdateHardware { () =>
    info.event match {
      case CC(cc) =>
        ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${light.isOn.currentValue()}")
        ext.midiOut.sendMidi(
          ShortMidiMessage.CONTROL_CHANGE + info.channel,
          cc,
          if (light.isOn.currentValue) 127 else 0)
      case Note(note) =>
        ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${light.isOn.currentValue()}")
        ext.midiOut.sendMidi(
          ShortMidiMessage.NOTE_ON + info.channel,
          note,
          if (light.isOn.currentValue) 127 else 0)
    }
  }
  light.isOn.setValue(false)
}

case class JamRgbButton(ext: MonsterJamExt, infoB: MidiInfo, infoL: MidiInfo) extends Button with RgbLight {
  val jamButton: JamButton = JamButton(ext, infoB)
  val jamLight: JamRgbLight = JamRgbLight(ext, infoL)

  val button: HardwareButton = jamButton.button
  val light: MultiStateHardwareLight = jamLight.light
  button.setBackgroundLight(light)
}

case class JamOnOffButton(ext: MonsterJamExt, info: MidiInfo) extends Button with OnOffLight {
  val jamButton: JamButton = JamButton(ext, info)
  val jamLight: JamOnOffLight = JamOnOffLight(ext, info)

  val button: HardwareButton = jamButton.button
  val light: OnOffHardwareLight = jamLight.light
  button.setBackgroundLight(light)
}

case class JamTouchStrip(ext: MonsterJamExt, touch: MidiInfo, slide: MidiInfo, led: MidiInfo) extends Button {
  val button: HardwareButton = JamButton(ext, touch).button
  val slider: HardwareSlider = ext.hw.createHardwareSlider(slide.id)

  // assume it's always CC
  val matcher = ext.midiIn.createAbsoluteCCValueMatcher(slide.channel, slide.event.value)
  slider.setAdjustValueMatcher(matcher)

  def update(value: Int): Unit = {
    //ext.host.println(s"updating slider ${slide} to $level")
    ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + slide.channel, slide.event.value, value)
  }

  var offsetCallback: Double => Unit = _ => ()
  def setOffsetCallback(f: Double => Unit): Unit = offsetCallback = f
  def clearOffsetCallback(): Unit = offsetCallback = _ => ()

  slider.value().addValueObserver(offsetCallback(_))
}

case class StripBank(ext: MonsterJamExt) extends Util {
  val strips: Vector[JamTouchStrip] = ('A' to 'H').map { idx =>
    JamTouchStrip(ext,
      touch = ext.xmlMap.button(s"CapTst$idx", ext.xmlMap.touchElems),
      slide = ext.xmlMap.knob(s"Tst$idx", ext.xmlMap.touchElems),
      led = ext.xmlMap.led(s"Tst${idx}IDX", ext.xmlMap.touchElems))

  }.toVector
    .forIndex(_.button.setIndexInGroup(_))
    .forIndex(_.slider.setIndexInGroup(_))

  var barMode: BarMode = BarMode.DUAL
  private val colors: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val values: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val active: mutable.ArraySeq[Boolean] = mutable.ArraySeq.fill(8)(false)

  def setColor(idx: Int, color: Int): Unit = {
    colors.update(idx, color)
    flushColors()
  }
  def setValue(idx: Int, value: Int): Unit = {
    values.update(idx, value)
    flushValues()
  }
  def setActive(idx: Int, value: Boolean): Unit = {
    active.update(idx, value)
    flushColors()
  }

  def flushColors(): Unit =
    ext.midiOut.sendSysex(createCommand("05",
      colors.zip(active).map { case (n, a) => f"${if (a) barMode.v else "00"}${n}%02x"}.mkString))
  def flushValues(): Unit = {
    //clear()
    ext.midiOut.sendSysex(createCommand("04", values.map(n => f"${n}%02x").mkString))
    //ext.host.println(active.toString())
    //ext.host.println(values.toString())
  }

  def clear(): Unit = ext.midiOut.sendSysex(BlackSysexMagic.zeroStrips)
}

// because real hardwarebuttons are not pressable programmatically
case class FakeAction(protected val invokeCallback:() => Unit) extends PolyAction {
  val callbacks: mutable.ArrayDeque[() => Unit] = mutable.ArrayDeque.empty
  def invoke(): Unit = {
    invokeCallback()
    callbacks.foreach(_())
  }
  def addBinding(f: () => Unit): Unit = callbacks.addOne(f)
  def addBinding(h: HardwareActionBindable): Unit = addBinding(() => h.invoke())
  def clearBindings(): Unit = callbacks.clear()
}
case class FakeButton() {
  var isPressed: Boolean = false
  val pressedAction: FakeAction = FakeAction(() => isPressed = true)
  val releasedAction: FakeAction = FakeAction(() => isPressed = false)
}
