package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.MonsterJamExt

class ColorManager(clipColor: => Color)(using ext: MonsterJamExt) {
  import JamColorBase.*
  val C = JamColorState
  object stepScene:
    def empty    = C(clipColor, 0)
    def selected = C(WHITE, 2)
    def nonEmpty = C(clipColor, 3)
    def playing  = C(WHITE, 0)

  object stepPad:
    private val noteRowRainbow = Vector(BLUE, PLUM, VIOLET, PURPLE)
    private def custom         = ext.preferences.altNoteRow.get()
    def playing                = C(WHITE, 1)
    def padColor(noteIdx: Int, step: NoteStep) =
      val bgColor = noteRowRainbow(noteIdx % 3)
      step.state() match
        case State.NoteOn      => if custom then C(WHITE, 3) else C(clipColor, 2)
        case State.Empty       => if custom then C(bgColor, 0) else C.empty
        case State.NoteSustain => C(WHITE, 0)

}
