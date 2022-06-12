package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.controller.api._
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.framework.binding
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.HB.HBS

import scala.language.implicitConversions
import com.github.unthingable.framework.binding.*
import com.github.unthingable.framework.HasId
import com.github.unthingable.jam.surface.KeyMaster.checkCombo
import com.github.unthingable.jam.surface.KeyMaster.RawButtonEvent

/*
Jam controls, self-wired to midi
 */

// universal interface, so that we can marry HardwareButton with FakeButton, make compiler happy and move on
trait ButtonStateSupplier:
  def isPressed: Boolean
  def press: ButtonEvt
  def release: ButtonEvt
  def pressedAction: HBS
  def releasedAction: HBS

object ButtonStateSupplier:
  def apply(id: String, btn: HardwareButton): ButtonStateSupplier = {
    btn.isPressed.markInterested()
    new ButtonStateSupplier {
      def isPressed: Boolean = btn.isPressed.get()
      val press: ButtonEvt = ButtonEvt.Press(id)
      val release: ButtonEvt = ButtonEvt.Release(id)
      val pressedAction = btn.pressedAction
      val releasedAction = btn.releasedAction
    }
  }

  def apply(btn: FakeButton): ButtonStateSupplier = new ButtonStateSupplier {
    def isPressed: Boolean = btn.isPressed
    val press: ButtonEvt = ButtonEvt.Press(btn.id)
    val release: ButtonEvt = ButtonEvt.Release(btn.id)
    val pressedAction = btn.pressedAction
    val releasedAction = btn.releasedAction
  }

trait HasButton[A]:
  def btn: A

trait HasButtonState:
  def st: ButtonStateSupplier

sealed trait HasHwButton extends HasButton[HardwareButton] 
  // extends Button:
// sealed trait HwButton extends Button[_]
  // def btn: HardwareButton
trait HasFakeButton extends HasButton[FakeButton]


sealed trait HasLight[L <: HardwareLight] { val light: L }
// trait ButtonLight[L <: HardwareLight] extends Light[L]
// trait OnOffButton extends ButtonLight[OnOffHardwareLight]
// trait RgbButton extends ButtonLight[MultiStateHardwareLight]
type HasOnOffLight = HasLight[OnOffHardwareLight]
type HasRgbLight = HasLight[MultiStateHardwareLight]

trait Info extends HasId { val info: MidiInfo; final val id: String = info.id }

object JamControl {

  /*  Wired hardware controls */

  import ActionDSL.action
  inline def handle(id: String, e: RawButtonEvent)(using ext: MonsterJamExt): HardwareActionBindable = 
    action("", () => KeyMaster.eval(id, e).foreach(ext.events.eval))
      
  def button(info: MidiInfo)(implicit ext: MonsterJamExt): HardwareButton = {
    val button: HardwareButton = ext.hw.createHardwareButton(info.id)

    val (on, off) = JamButton.infoActionMatchers(info)

    button.pressedAction.setActionMatcher(on)
    button.releasedAction.setActionMatcher(off)
    button.isPressed.markInterested()

    button.pressedAction.addBinding(handle(info.id, RawButtonEvent.Press))
    button.releasedAction.addBinding(handle(info.id, RawButtonEvent.Release))

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

  def maybeLight(b: Any): Option[OnOffHardwareLight] = 
    b match 
      // let's assume that's the only one, it's a sealed trait after all
      case x: JamOnOffButton => Some(x.light)
      case _ => None
}

object JamButton {
  def infoActionMatchers(info: MidiInfo)(using ext: MonsterJamExt): (HardwareActionMatcher, HardwareActionMatcher) =
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

abstract class JamButtonLight[L <: HardwareLight](id: String, btn: HardwareButton, light: L)
  extends HasButtonState, HasLight[L], HasHwButton, HasId {
    val st: ButtonStateSupplier = ButtonStateSupplier(id, btn)
  }

case class JamRgbButton(id: String, btn: HardwareButton, light: MultiStateHardwareLight) 
  extends JamButtonLight(id, btn, light)

case class JamOnOffButton(id: String, btn: HardwareButton, light: OnOffHardwareLight) 
  extends JamButtonLight(id, btn, light)

class JamTouchStrip(touch: MidiInfo, slide: MidiInfo, led: MidiInfo)(using ext: MonsterJamExt) {
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

