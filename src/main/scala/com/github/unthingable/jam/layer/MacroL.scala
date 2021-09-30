package com.github.unthingable.jam.layer

import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.{Binding, GateMode, HB, IntActivatedLayer, Jam, ModeButtonLayer, SupColorStateB}

trait MacroL { this: Jam =>
  lazy val macroLayer = new ModeButtonLayer("MACRO", j.macroButton, GateMode.Gate, silent = true) {
    var bumpedStrip  : Option[IntActivatedLayer] = None
    var bumpedSubMode: Option[Int]               = None

    override def activate(): Unit = {
      super.activate()
      // dirty hack to show user controls
      if (j.control.isPressed())
        controlLayer.cycle()
      else {
        bumpedStrip = stripGroup.layers.find(_.isOn).collect { case x: IntActivatedLayer => x }.filter(_ != controlLayer)
        if (!controlLayer.selected.contains(1)) {
          bumpedSubMode = controlLayer.selected
          controlLayer.select(1)
        }
        if (!controlLayer.isOn) controlLayer.activateAction.invoke()
      }
    }

    override def deactivate(): Unit = {
      bumpedStrip.foreach(_.activateAction.invoke())
      bumpedSubMode.foreach(controlLayer.select)
      bumpedStrip = None
      bumpedSubMode = None
      super.deactivate()
    }

    override val modeBindings: Seq[Binding[_, _, _]] =
      (0 until superBank.getCapacityOfBank.min(64)).flatMap { superIdx =>
        val track      = superBank.getItemAt(superIdx)
        val row        = superIdx / 8
        val col        = superIdx % 8
        val btn        = j.matrix(row)(col)
        val isSelected = ext.cursorTrack.createEqualsValue(track)
        isSelected.markInterested()

        Vector(
          SupColorStateB(btn.light, () =>
            if (isSelected.get())
              JamColorState(JAMColorBase.WHITE, 3)
            else
              JamColorState(track.color().get(), 0)),
          HB(btn.pressedAction, "direct select track", () => ext.cursorTrack.selectChannel(track)),
          HB(btn.releasedAction, "direct select release", () => ()),
        )
      }
  }
}
