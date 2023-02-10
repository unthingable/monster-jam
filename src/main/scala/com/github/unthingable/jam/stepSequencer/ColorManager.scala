package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamSurface

given Util.SelfEqual[NoteStep.State] = CanEqual.derived

class ColorManager(clipColor: => Color)(using ext: MonsterJamExt):
  import JamColorBase.*
  val C = JamColorState
  object stepScene:
    def playing(using j: JamSurface) = C(WHITE, if j.Mod.blinkTempo then 3 else 0)
    val selected                     = C(WHITE, 3)
    val nonEmpty                     = C(WHITE, 0)
    def empty                        = C(clipColor, 0)

  object stepPad:
    private val noteRowRainbow = Vector(BLUE, PLUM, VIOLET, PURPLE)
    private def custom         = ext.preferences.altNoteRow.get()
    val playing                = C(WHITE, 1)
    val rowSelected            = C(WHITE, 1)
    def noteColor(noteIdx: Int): Int | Color =
      if custom then noteRowRainbow(noteIdx % 3) else clipColor
    def padColor(noteIdx: Int, step: NoteStep) =
      step.state() match
        case State.NoteOn      => C(noteColor(noteIdx), 2)
        case State.Empty       => C.empty
        case State.NoteSustain => C(WHITE, 0)
end ColorManager
