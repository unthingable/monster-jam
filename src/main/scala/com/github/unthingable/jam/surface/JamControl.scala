package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.controller.api._
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.framework.binding
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.HB.HBS

import scala.language.implicitConversions

/*
Jam controls, self-wired to midi
 */

trait ButtonActionSupplier {
  def pressed : HBS
  def released: HBS
  def isPressed     : () => Boolean
}

object ButtonActionSupplier {
  def apply(_pressed: HBS, _released: HBS, _isPressed: () => Boolean): ButtonActionSupplier = new ButtonActionSupplier {
    override val pressed: HBS = _pressed
    override val released: HBS = _released
    override val isPressed: () => Boolean = _isPressed
  }
}

sealed trait Button {
  def btn: ButtonActionSupplier
}

sealed trait HwButton {
  def hwb: HardwareButton
}

sealed trait Light[L <: HardwareLight] { val light: L }
trait ButtonLight[L <: HardwareLight] extends Button with Light[L]
trait OnOffButton extends ButtonLight[OnOffHardwareLight]
trait RgbButton extends ButtonLight[MultiStateHardwareLight]

trait HasId {
  def id: String
  // IDs shouldn't repeat among the same set of objects
  override def hashCode(): Int = id.hashCode()
}

trait Info extends HasId {val info: MidiInfo; final val id: String = info.id }

object JamControl {

  /*  Wired hardware controls */

  def button(info: MidiInfo)(implicit ext: MonsterJamExt): HardwareButton = {
    val button: HardwareButton = ext.hw.createHardwareButton(info.id)

    val (on, off) = JamButton.infoActionMatchers(info)

    button.pressedAction.setActionMatcher(on)
    button.releasedAction.setActionMatcher(off)
    button.isPressed.markInterested()

    button
  }

  def rgbLight(info: MidiInfo)(implicit ext: MonsterJamExt): MultiStateHardwareLight = {
    import JamColorState._
    val light            : MultiStateHardwareLight = ext.hw.createMultiStateHardwareLight(info.id + "_LED")
    var updatedColorState: JamColorState           = JamColorState.empty

    light.setColorToStateFunction(toState)
    light.state().onUpdateHardware { (state: JamColorState) =>
      updatedColorState = state
      sendColor(state.value)
    }
    light.state().setValue(JamColorState.empty)

    def toState(color: Color): InternalHardwareLightState = JamColorState(toColorIndex(color), updatedColorState.brightness)

    def sendColor(color: Int): Unit = {
      info.event match {
        case CC(cc)     =>
          //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${color.toString}")
          ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, cc, color)
        case Note(note) =>
          //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${color.toString}")
          ext.midiOut.sendMidi(ShortMidiMessage.NOTE_ON + info.channel, note, color)
      }
    }
    light
  }

  def onOffLight(info: MidiInfo)(implicit ext: MonsterJamExt): OnOffHardwareLight = {
    val light: OnOffHardwareLight = ext.hw.createOnOffHardwareLight(info.id + "_LED")
    light.onUpdateHardware { () =>
      info.event match {
        case CC(cc)     =>
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
    light
  }

  implicit class HbOps(b: HardwareButton) {
    def asHas: ButtonActionSupplier =
      ButtonActionSupplier(b.pressedAction, b.releasedAction, b.isPressed.get)
  }
}

//case class JamButton(info: MidiInfo)(implicit ext: MonsterJamExt, surfaceState: SurfaceState) extends ButtonActionSupplier with Info {
//  protected[surface] val button: HardwareButton = ext.hw.createHardwareButton(info.id)
//
//  val (on, off) = JamButton.infoActionMatchers(info)
//
//  button.pressedAction.setActionMatcher(on)
//  button.releasedAction.setActionMatcher(off)
//  button.isPressed.markInterested()
//
//  val raw: ButtonActions = ButtonActions(button.pressedAction, button.releasedAction, button.isPressed.get)
//
//  override val pressedAction : HB.HBS        = button.pressedAction
//  override val releasedAction: HB.HBS        = button.releasedAction
//  override val isPressed     : () => Boolean = button.isPressed.get
//
//  override val pressedNC = FakeAction()
//  override val releasedNC = FakeAction()
//
//  button.pressedAction.addBinding(ext.a(if (shouldFire) pressedNC.invoke()))
//  button.releasedAction.addBinding(ext.a(if (shouldFire) releasedNC.invoke()))
//
//  private def shouldFire: Boolean = surfaceState.comboMap.get(info.id).exists(_.exists(_.isPressedAny()))
//}

object JamButton {
  def infoActionMatchers(info: MidiInfo)(implicit ext: MonsterJamExt): (HardwareActionMatcher, HardwareActionMatcher) =
    info.event match {
      case CC(cc)     =>
        (
          ext.midiIn.createCCActionMatcher(info.channel, cc, 127),
          ext.midiIn.createCCActionMatcher(info.channel, cc, 0)
        )
      case Note(note) =>
        (
          ext.midiIn.createNoteOnActionMatcher(info.channel, note),
          ext.midiIn.createNoteOffActionMatcher(info.channel, note)
        )
    }
}

/* These represent actual hardware buttons, with one HardwareButton with raw events and one light.
 * Sometimes you need to work with raw button actions and states, so they are there for you.
 * These are declared once in JamSurface and IDs are unique, which is why we can hash them.
 * */
case class JamRgbButton(id: String, btn: ButtonActionSupplier, light: MultiStateHardwareLight) extends RgbButton with HasId

case class JamOnOffButton(id: String, btn: ButtonActionSupplier, light: OnOffHardwareLight) extends OnOffButton with HasId

class JamTouchStrip(touch: MidiInfo, slide: MidiInfo, led: MidiInfo)(implicit ext: MonsterJamExt) {
  val slider: HardwareSlider = ext.hw.createHardwareSlider(slide.id)
  slider.isBeingTouched.markInterested()

  // assume it's always CC
  val matcher = ext.midiIn.createAbsoluteCCValueMatcher(slide.channel, slide.event.value)
  slider.setAdjustValueMatcher(matcher)

  val (on, off) = JamButton.infoActionMatchers(touch)
  slider.beginTouchAction().setActionMatcher(on)
  slider.endTouchAction().setActionMatcher(off)

  def update(value: Int): Unit = {
    //ext.host.println(s"updating slider ${slide} to $level")
    ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + slide.channel, slide.event.value, value)
  }

  // such speedup
  override def hashCode(): Int = touch.event.value
}

