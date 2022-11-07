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
    val selected = C(WHITE, 2)
    val playing  = C(WHITE, 0)
    def nonEmpty = C(clipColor, 3)
    def empty    = C(clipColor, 0)

  object stepPad:
    private val noteRowRainbow = Vector(BLUE, PLUM, VIOLET, PURPLE)
    private def custom         = ext.preferences.altNoteRow.get()
    def playing                = C(WHITE, 1)
    def noteColor(noteIdx: Int): Int | Color =
      if custom then noteRowRainbow(noteIdx % 3) else clipColor
    def padColor(noteIdx: Int, step: NoteStep) =
      step.state() match
        case State.NoteOn      => if custom then C(WHITE, 3) else C(clipColor, 2)
        case State.Empty       => if custom then C(noteColor(noteIdx), 0) else C.empty
        case State.NoteSustain => C(WHITE, 0)

}
