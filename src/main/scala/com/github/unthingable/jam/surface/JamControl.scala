package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.controller.api._
import com.github
import com.github.unthingable
import com.github.unthingable.jam.binding
import com.github.unthingable.jam.binding.HB
import com.github.unthingable.jam.binding.HB.HBS
import com.github.unthingable.{MonsterJamExt, Util, jam}
import com.github.unthingable.jam.surface.BlackSysexMagic.{BarMode, createCommand}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try

/*
Jam controls, self-wired to midi
 */

case class ButtonActions(pressed: HBS, released: HBS, isPressed: () => Boolean)

trait ButtonActionSupplier {
  def pressedAction : HBS
  def releasedAction: HBS
  def isPressed     : () => Boolean
}

sealed trait Button {
  def btn: ButtonActionSupplier
}

sealed trait HwButton {
  def hwb: HardwareButton
}

trait SurfaceState {
  /**
   * A button and all its combo neighbors
   */
  val comboMap: mutable.Map[String, Set[Combo.JC]] = mutable.Map.empty
}

// key chords
object Combo {
  implicit private val surfaceState: SurfaceState = new SurfaceState {}

  type NamedButton = Button with HasName
  // a button combo is like a micro mode that's always on
  case class JC(b1: NamedButton, bb: NamedButton*)(implicit ext: MonsterJamExt) {
    val buttons: Seq[NamedButton] = b1 +: bb

    buttons.foreach { b =>
      b.raw.pressed.addBinding(ext.a(onPress(b)))
      b.raw.released.addBinding(ext.a(onRelease(b)))
      surfaceState.comboMap.updateWith(b.name)(_.map(cc => cc + this))
    }

    private var keysOn: Int     = 0
    private var allOn : Boolean = false

    val pressed     = FakeAction() // all combo buttons pressed
    val releasedOne = FakeAction() // combo no longer fully held
    val releasedAll = FakeAction() // all combo buttons released

    val isPressedAll: () => Boolean = () => keysOn == buttons.size
    val isPressedAny: () => Boolean = () => keysOn > 0

    private def onPress(b: Button): Unit = {
      val newState = keysOn + 1
      if (newState == buttons.size) {
        if (!allOn) {
          pressed.invoke()
          allOn = true
        }
      }
      keysOn = newState
    }

    private def onRelease(b: Button): Unit = {
      val newState = keysOn - 1
      if (newState == buttons.size - 1)
        releasedOne.invoke()
      if (newState == 0 && allOn) {
        releasedAll.invoke()
        allOn = false
      }
      keysOn = newState
    }
  }
}

trait Light[L <: HardwareLight] { val light: L }
trait RgbLight extends Light[MultiStateHardwareLight]
trait OnOffLight extends Light[OnOffHardwareLight]
trait OnOffButton extends Button with OnOffLight

trait HasName { def name: String }
trait Info extends HasName { val info: MidiInfo; val name: String = info.id }

/**
 * Hardware Jam button, actions triggered by raw events
 */

object JamControl {
  //type ButtonActionSupplier = {
  //  def pressedAction : HBS
  //  def releasedAction: HBS
  //  def isPressed     : () => Boolean
  //}

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
    light.state().onUpdateHardware { state: JamColorState =>
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
    def asHas: ButtonActionSupplier = new ButtonActionSupplier {
      override def pressedAction: HB.HBS = b.pressedAction
      override def releasedAction: HB.HBS = b.releasedAction
      override def isPressed: () => Boolean = b.isPressed.get
    }
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

/**
 * Hardware Jam RGB light
 */
//case class JamRgbLight(info: MidiInfo)(implicit ext: MonsterJamExt) extends RgbLight with Info {
//  import JamColorState._
//  val light: MultiStateHardwareLight = ext.hw.createMultiStateHardwareLight(info.id + "_LED")
//  var updatedColorState: JamColorState = JamColorState.empty
//
//  light.setColorToStateFunction(toState)
//  light.state().onUpdateHardware { state: JamColorState =>
//    updatedColorState = state
//    sendColor(state)
//  }
//  light.state().setValue(JamColorState.empty)
//
//  private def toState(color: Color): InternalHardwareLightState = JamColorState(toColorIndex(color), updatedColorState.brightness)
//
//  private def sendColor(color: JamColorState): Unit = sendColor(color.value)
//
//  private def sendColor(color: Int): Unit = {
//    info.event match {
//      case CC(cc) =>
//        //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${color.toString}")
//        ext.midiOut.sendMidi(ShortMidiMessage.CONTROL_CHANGE + info.channel, cc, color)
//      case Note(note) =>
//        //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${color.toString}")
//        ext.midiOut.sendMidi(ShortMidiMessage.NOTE_ON + info.channel, note, color)
//    }
//  }
//}

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

/**
 * Hardware Jam on/off light
 */
//case class JamOnOffLight(info: MidiInfo)(implicit ext: MonsterJamExt) extends Info {
//  val light: OnOffHardwareLight = ext.hw.createOnOffHardwareLight(info.id + "_LED")
//  light.onUpdateHardware { () =>
//    info.event match {
//      case CC(cc) =>
//        //ext.host.println(s"${info.id} setting CC ${info.channel} ${cc} ${light.isOn.currentValue()}")
//        ext.midiOut.sendMidi(
//          ShortMidiMessage.CONTROL_CHANGE + info.channel,
//          cc,
//          if (light.isOn.currentValue) 127 else 0)
//      case Note(note) =>
//        //ext.host.println(s"${info.id} setting NOTE ${info.channel} ${note} ${light.isOn.currentValue()}")
//        ext.midiOut.sendMidi(
//          ShortMidiMessage.NOTE_ON + info.channel,
//          note,
//          if (light.isOn.currentValue) 127 else 0)
//    }
//  }
//  light.isOn.setValue(false)
//}

case class JamRgbButton(btn: ButtonActionSupplier, light: MultiStateHardwareLight) extends Button with RgbLight

case class JamOnOffButton(btn: ButtonActionSupplier, light: OnOffHardwareLight) extends OnOffButton

//case class JamRgbButton(infoB: MidiInfo, infoL: MidiInfo)(implicit ext: MonsterJamExt, st: SurfaceState) extends Button with RgbLight with Info {
//  val info = infoB
//  val jamButton: JamButton = JamButton(infoB)
//  val jamLight: JamRgbLight = JamRgbLight(infoL)
//
//  val light: MultiStateHardwareLight = jamLight.light
//  jamButton.button.setBackgroundLight(light)
//
//  override val pressedAction : HB.HBS        = jamButton.pressedAction
//  override val releasedAction: HB.HBS        = jamButton.releasedAction
//  override val isPressed     : () => Boolean = jamButton.isPressed
//  override val pressedNC     : HB.HBS = jamButton.pressedNC
//  override val releasedNC    : HB.HBS = jamButton.releasedNC
//}
//
//case class JamOnOffButton(info: MidiInfo)(implicit ext: MonsterJamExt, st: SurfaceState) extends OnOffButton with Info {
//  val jamButton: JamButton = JamButton(info)
//  val jamLight: JamOnOffLight = JamOnOffLight(info)
//
//  val light: OnOffHardwareLight = jamLight.light
//  jamButton.button.setBackgroundLight(light)
//
//  override val pressedAction : HB.HBS        = jamButton.pressedAction
//  override val releasedAction: HB.HBS        = jamButton.releasedAction
//  override val isPressed     : () => Boolean = jamButton.isPressed
//  override val pressedNC     : HB.HBS = jamButton.pressedNC
//  override val releasedNC    : HB.HBS = jamButton.releasedNC
//}

case class JamTouchStrip(touch: MidiInfo, slide: MidiInfo, led: MidiInfo)(implicit ext: MonsterJamExt) {
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

class StripBank()(implicit ext: MonsterJamExt) extends Util {
  val strips: Vector[JamTouchStrip] = ('A' to 'H').map { idx =>
    JamTouchStrip(
      touch = ext.xmlMap.button(s"CapTst$idx", ext.xmlMap.touchElems),
      slide = ext.xmlMap.knob(s"Tst$idx", ext.xmlMap.touchElems),
      led = ext.xmlMap.led(s"Tst${idx}IDX", ext.xmlMap.touchElems))

  }.toVector
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
 */
class FakeAction
  extends HardwareBindingSource[HardwareActionBinding] with Util {
  protected val invokeCallback: () => Unit = () => ()

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

object FakeAction {
  def apply() = new FakeAction
  // when two dynamically created actions are same
  def apply(hashString: String): FakeAction = new FakeAction {
    override def hashCode(): Int = hashString.hashCode
  }
}

case class FakeButton() {
  var isPressed: Boolean = false
  val pressedAction: FakeAction = new FakeAction { override val invokeCallback = () => isPressed = true }
  val releasedAction: FakeAction = new FakeAction { override val invokeCallback = () => isPressed = false }
}
