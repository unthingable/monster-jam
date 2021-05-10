package com.github.unthingable.jam

package object surface {
  trait SurfaceElement
  trait HasColor

  sealed trait MidiEvent { val value: Int }
  case class CC(value: Int) extends MidiEvent
  case class Note(value: Int) extends MidiEvent
  //case class Poly(value: Int) extends MidiEvent
  case class MidiInfo(id: String, channel: Int, event: MidiEvent)

  // Basic elements
  trait Indicator extends SurfaceElement
  case class Light(info: MidiInfo) extends Indicator
  case class RgbLight(info: MidiInfo) extends Indicator with HasColor
  trait Meter extends Indicator
  trait RgbMeter extends Meter with HasColor

  trait Control extends SurfaceElement
  case class Button(info: MidiInfo) extends Control
  trait Knob extends Control
  trait Touchstrip extends Control



  // Composite elements
  //case class LitButtonInfo(button: MidiInfo, light: MidiInfo)
  //case class ButtonMidiInfo(button: MidiInfo, light: MidiInfo)
}
