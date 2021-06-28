package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.controller.api._
import com.github.unthingable.jam.HB
import com.github.unthingable.jam.HB.HBS
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface.BlackSysexMagic.{BarMode, createCommand}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try

/*
Jam controls, self-wired to midi
 */

sealed trait JamControl


trait Button extends JamControl {
  val pressedAction: HBS

  val releasedAction: HBS

  val isPressed: () => Boolean
}
trait Light[L <: HardwareLight] { val light: L }
trait RgbLight extends Light[MultiStateHardwareLight]
trait OnOffLight extends Light[OnOffHardwareLight]
trait OnOffButton extends Button with OnOffLight

case class JamButton(info: MidiInfo)(implicit ext: MonsterJamExt) extends Button {
  protected[surface] val button: HardwareButton = ext.hw.createHardwareButton(info.id)

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
  button.isPressed.markInterested()

  override val pressedAction : HB.HBS        = button.pressedAction
  override val releasedAction: HB.HBS        = button.releasedAction
  override val isPressed     : () => Boolean = button.isPressed.get
}


case class JamRgbLight(info: MidiInfo)(implicit ext: MonsterJamExt) extends RgbLight {
  import JamColorState._
  val light: MultiStateHardwareLight = ext.hw.createMultiStateHardwareLight(info.id + "_LED")
  var updatedColorState: JamColorState = JamColorState.empty

  light.setColorToStateFunction(toState)
  light.state().onUpdateHardware { state: JamColorState =>
    updatedColorState = state
    sendColor(state)
  }
  light.state().setValue(JamColorState.empty)

  def toState(color: Color): InternalHardwareLightState = JamColorState(toColorIndex(color), updatedColorState.brightness)

  def sendColor(color: JamColorState): Unit = sendColor(color.value)

  def sendColor(color: Int): Unit = {
    info.event match {
      case CC(cc) =>
        //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${color.toString}")
        ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, cc, color)
      case Note(note) =>
        //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${color.toString}")
        ext.midiOut.sendMidi(ShortMidiMessage.NOTE_ON + info.channel, note, color)
    }
  }
}

case class JamColorState(color: Int, brightness: Int) extends InternalHardwareLightState {
  override def getVisualState: HardwareLightVisualState = null
  val value: Int = if (brightness >= 0) color + brightness else 0
}

object JamColorState {
  val empty: JamColorState = JamColorState(0, 0)

  def apply(color: Color, brightness: Int): JamColorState = JamColorState(toColorIndex(color), brightness)

  def toColorIndex(color: Color): Int =
    NIColorUtil.convertColor(color.getRed.toFloat, color.getGreen.toFloat, color.getBlue.toFloat)
}

case class JamOnOffLight(info: MidiInfo)(implicit ext: MonsterJamExt) {
  val light: OnOffHardwareLight = ext.hw.createOnOffHardwareLight(info.id + "_LED")
  light.onUpdateHardware { () =>
    info.event match {
      case CC(cc) =>
        //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${light.isOn.currentValue()}")
        ext.midiOut.sendMidi(
          ShortMidiMessage.CONTROL_CHANGE + info.channel,
          cc,
          if (light.isOn.currentValue) 127 else 0)
      case Note(note) =>
        //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${light.isOn.currentValue()}")
        ext.midiOut.sendMidi(
          ShortMidiMessage.NOTE_ON + info.channel,
          note,
          if (light.isOn.currentValue) 127 else 0)
    }
  }
  light.isOn.setValue(false)
}

case class JamRgbButton(infoB: MidiInfo, infoL: MidiInfo)(implicit ext: MonsterJamExt) extends Button with RgbLight {
  val jamButton: JamButton = JamButton(infoB)
  val jamLight: JamRgbLight = JamRgbLight(infoL)

  protected[surface] val button: HardwareButton = jamButton.button
  val light: MultiStateHardwareLight = jamLight.light
  button.setBackgroundLight(light)

  override val pressedAction : HB.HBS        = button.pressedAction
  override val releasedAction: HB.HBS        = button.releasedAction
  override val isPressed     : () => Boolean = button.isPressed.get
}

case class JamOnOffButton(info: MidiInfo)(implicit ext: MonsterJamExt) extends OnOffButton {
  val jamButton: JamButton = JamButton(info)
  val jamLight: JamOnOffLight = JamOnOffLight(info)

  protected[surface] val button: HardwareButton = jamButton.button
  val light: OnOffHardwareLight = jamLight.light
  button.setBackgroundLight(light)

  override val pressedAction : HB.HBS        = button.pressedAction
  override val releasedAction: HB.HBS        = button.releasedAction
  override val isPressed     : () => Boolean = button.isPressed.get
}

case class JamTouchStrip(touch: MidiInfo, slide: MidiInfo, led: MidiInfo)(implicit ext: MonsterJamExt) extends Button {
  val button: HardwareButton = JamButton(touch).button
  val slider: HardwareSlider = ext.hw.createHardwareSlider(slide.id)

  // assume it's always CC
  val matcher = ext.midiIn.createAbsoluteCCValueMatcher(slide.channel, slide.event.value)
  slider.setAdjustValueMatcher(matcher)

  def update(value: Int): Unit = {
    //ext.host.println(s"updating slider ${slide} to $level")
    ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + slide.channel, slide.event.value, value)
  }

  override val pressedAction : HB.HBS        = button.pressedAction
  override val releasedAction: HB.HBS        = button.releasedAction
  override val isPressed     : () => Boolean = button.isPressed.get

  // such speedup
  override def hashCode(): Int = touch.event.value
}

case class StripBank()(implicit ext: MonsterJamExt) extends Util {
  val strips: Vector[JamTouchStrip] = ('A' to 'H').map { idx =>
    JamTouchStrip(
      touch = ext.xmlMap.button(s"CapTst$idx", ext.xmlMap.touchElems),
      slide = ext.xmlMap.knob(s"Tst$idx", ext.xmlMap.touchElems),
      led = ext.xmlMap.led(s"Tst${idx}IDX", ext.xmlMap.touchElems))

  }.toVector
    .forindex(_.button.setIndexInGroup(_))
    .forindex(_.slider.setIndexInGroup(_))

  var barMode: BarMode = BarMode.DUAL
  private val colors: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val values: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val active: mutable.ArraySeq[Boolean] = mutable.ArraySeq.fill(8)(false)

  def setColor(idx: Int, color: Int, flush: Boolean = true): Unit = {
    // some more NI magic for you, white is not the same for strips. Also we can't really show black tracks.
    colors.update(idx, if (color == 68 || color == 0) 120 else color)
    if (flush) flushColors()
  }
  def setValue(idx: Int, value: Int, flush: Boolean = true): Unit = {
    if (barMode == BarMode.DUAL) {
      values.update(idx, value)
      if (flush) flushValues()
    } else strips(idx).update(value)
  }
  def setActive(idx: Int, value: Boolean, flush: Boolean = true): Unit = {
    active.update(idx, value)
    if (flush) flushColors()
  }

  def flushColors(): Unit =
    ext.midiOut.sendSysex(createCommand("05",
      colors.zip(active).map { case (n, a) => f"${if (a) barMode.v else "00"}${n}%02x"}.mkString))
  def flushValues(): Unit = {
    ext.midiOut.sendSysex(createCommand("04", values.map(n => f"${n}%02x").mkString))
  }

  def clear(): Unit = ext.midiOut.sendSysex(BlackSysexMagic.zeroStrips)
}

/**
 * An explicitly invokable action/HardwareBindingSource. Use to construct hardware controls or as generic
 * subscribable action.
 *
 * Being a HardwareBindingSource allows this to be used anywhere a real action is required.
 *
 * @param invokeCallback always called when action is invoked, never cleared
 * @param masquerade true when representing a real button
 */
case class FakeAction(protected val invokeCallback:() => Unit = () => (), masquerade: Boolean = false)
  extends HardwareBindingSource[HardwareActionBinding] with Util {
  val callbacks = mutable.LinkedHashSet.empty[HardwareActionBindable]
  def invoke(): Unit = {
    invokeCallback()
    Array.from(callbacks).zipWithIndex.foreach { case (f, idx) =>
      //assert(idx < callbacks.size)
      //Util.println(s"calling $idx of ${callbacks.size}")
      f.invoke()
    }
  }
  def addBinding(f: HardwareActionBindable): HardwareActionBinding = {
    callbacks.addOne(f)
    () => callbacks.remove(f)
  }
  def setBinding(f: HardwareActionBindable): HardwareActionBinding = {
    callbacks.clear()
    addBinding(f)
  }
  def clearBindings(): Unit = callbacks.clear()

  override def canBindTo(o: Any): Boolean = o match {
    //case _: Runnable => true
    case _: HardwareActionBindable => true
    case _ => false
  }

  override def addBinding(h: HardwareBindable): HardwareActionBinding = h match {
    case hab: HardwareActionBindable => addBinding(hab)
    case _ => ??? // better be never
  }

  override def setBinding(h: HardwareBindable): HardwareActionBinding = h match {
    case hab: HardwareActionBindable => setBinding(hab)
    case _ => ???
  }
}
case class FakeButton() {
  var isPressed: Boolean = false
  val pressedAction: FakeAction = FakeAction(() => isPressed = true, masquerade = true)
  val releasedAction: FakeAction = FakeAction(() => isPressed = false, masquerade = true)
}
