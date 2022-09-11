package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.Send
import com.github.unthingable.Util
import com.github.unthingable.framework.binding.{Binding, EB, SupColorStateB}
import com.github.unthingable.framework.mode.{CycleMode, GateMode, ModeButtonCycleLayer, ModeButtonLayer}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.{Jam, SliderBankMode}

trait Aux { this: Jam =>
  lazy val auxLayer = new ModeButtonCycleLayer("AUX", j.aux, CycleMode.Select) with Util {
    override val subModes = EIGHT.map(idx =>
      new SliderBankMode("strips aux", trackBank.getItemAt(_).sendBank().getItemAt(idx), identity, Seq.fill(8)(BarMode.SINGLE))
      )
  }

  lazy val auxGate = new ModeButtonLayer("strip AUX gate", j.aux,
    GateMode.Gate,
    silent = true
  ) {
    val bank = ext.host.createEffectTrackBank(8, 1)
    override val modeBindings: Seq[Binding[_, _, _]] =
      j.groupButtons.zipWithIndex.flatMap { case (b, idx) =>
        val color = bank.getItemAt(idx).color()
        color.markInterested()

        Vector(
          EB(b.st.press, s"aux select $idx", () => auxLayer.select(idx)),
          SupColorStateB(b.light, () =>
            (if (auxLayer.selected.contains(idx))
               JamColorState(JamColorBase.WHITE, 3)
             else
               JamColorState(color.get(), 0)),
            JamColorState.empty))
      }
  }
}
