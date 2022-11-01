package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState

class ColorManager(clipColor: => Color) {
  import JamColorBase.*
  val C = JamColorState
  object stepScene:
    def empty    = C(clipColor, 0)
    def selected = C(WHITE, 2)
    def nonEmpty = C(clipColor, 3)
    def playing  = C(WHITE, 0)

  object stepPad:
    private val noteRowRainbow = Vector(BLUE, PLUM, VIOLET, PURPLE)
    def playing                = C(WHITE, 1)
    def padColor(noteIdx: Int, step: NoteStep) =
      val bgColor = noteRowRainbow(noteIdx % 2)
      step.state() match
        case State.NoteOn      => C(WHITE, 3)
        case State.Empty       => C(bgColor, 0)
        case State.NoteSustain => C(WHITE, 0)

}
