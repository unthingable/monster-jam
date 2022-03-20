package com.github.unthingable.jam.surface

import com.bitwig.extension.api.util.midi.ShortMidiMessage
import com.bitwig.extension.callback.IntegerValueChangedCallback
import com.bitwig.extension.controller.api.{HardwareButton, OnOffHardwareLight, RelativeHardwareKnob}
import com.github.unthingable.jam.binding.HB
import com.github.unthingable.jam.surface.JamControl.HbOps
import com.github.unthingable.jam.{binding, surface}
import com.github.unthingable.{MonsterJamExt, Util}


/* Surface model with all the controls, wired to MIDI */

trait HasButton { val button: FakeButton }

class JamSurface(implicit ext: MonsterJamExt) extends Util {
  implicit private val surfaceState = new SurfaceState {}

  private def b(id: String) = {
    val info = ext.xmlMap.button(id)
    val button = JamControl.button(info)
    val led = JamControl.onOffLight(info)
    JamOnOffButton(button.asHas, led)
  }

  object Mod {
    var Shift: OnOffButton with HasButton with HasName = new OnOffButton with HasButton with HasName {
      val name = "SHIFT"
      override val button: FakeButton         = FakeButton()
      override val light : OnOffHardwareLight = ext.hw.createOnOffHardwareLight("shift_LED") // fake
      override val pressedAction: HB.HBS      = button.pressedAction
      override val releasedAction: HB.HBS     = button.releasedAction
      override val isPressed: () => Boolean   = () => button.isPressed

      // an inelegant repetition of JamButton, but this is the pattern we've got
      override val pressedNC = FakeAction()
      override val releasedNC = FakeAction()

      button.pressedAction.addBinding(ext.a(if (shouldFire) pressedNC.invoke()))
      button.releasedAction.addBinding(ext.a(if (shouldFire) releasedNC.invoke()))

      private def shouldFire: Boolean = surfaceState.comboMap.get(name).exists(_.exists(_.isPressedAny()))
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
      val info = ext.xmlMap.button(s"BtnDpad$idx")
      val button = JamControl.button(info)
      val led = JamControl.onOffLight(info)
      JamOnOffButton(button.asHas, led)
    }

    val up: JamOnOffButton = b(1)
    val down: JamOnOffButton = b(3)
    val left: JamOnOffButton = b(4)
    val right: JamOnOffButton = b(2)
  }

  /* Main section */

  val sceneButtons: Seq[JamRgbButton] = (1 to 8).map { idx =>
    val btnInfo = ext.xmlMap.button(s"BtnScene${idx}")
    // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
    val btnLedInfo = ext.xmlMap.led(s"BtnScene${idx}IDX").copy(channel = btnInfo.channel)

    val button = JamControl.button(btnInfo)
    val led    = JamControl.rgbLight(btnLedInfo)

    button.setIndexInGroup(idx)

    JamRgbButton(button.asHas, led)
  }

  val matrix: Seq[Seq[JamRgbButton]] = (1 to 8).map { row =>
    ('A' to 'H').map { col =>
      val btnInfo = ext.xmlMap.button(s"Btn$col$row", ext.xmlMap.matrixElems)
      val ledInfo = ext.xmlMap.led(s"Btn$col${row}IDX", ext.xmlMap.matrixElems)

      val button = JamControl.button(btnInfo)
      val led = JamControl.rgbLight(ledInfo)

      JamRgbButton(button.asHas, led)
    }
  }

  val groupButtons: Seq[JamRgbButton] = ('A' to 'H').map { idx =>
    val btnInfo = ext.xmlMap.button(s"BtnGroup${idx}")
    // mapping says channel 0 for IDX led, but it works when it's 1 (same as button)
    val ledInfo = ext.xmlMap.led(s"BtnGroup${idx}IDX").copy(channel = btnInfo.channel)

    val button = JamControl.button(btnInfo)
    val led = JamControl.rgbLight(ledInfo)

    button.setIndexInGroup(idx)

    JamRgbButton(button.asHas, led)
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
        Mod.Shift.button.pressedAction.invoke()
      case ShiftReleaseCommand =>
        Mod.Shift.button.releasedAction.invoke()
      case ReturnFromHostCommand =>
        ext.host.println("return from host")
        ext.hw.invalidateHardwareOutputState()
        ext.host.requestFlush()
      case x => "Unhandled sysex from controller: " + ext.host.println(x)
    }
  }

  ///**
  // * A button and all its combo neighbors
  // */
  //private val comboMap: mutable.Map[Button, Set[Button]] = mutable.Map.empty
  ///**
  // * A version of the button that only fires when none of its combo members are pressed
  // */
  //private val onlyCache: mutable.Map[Button, Button] = mutable.Map.empty


  import Combo.JC

  val ShiftDup = JC(Mod.Shift, duplicate)
  val ShiftSolo = JC(Mod.Shift, solo)
}
