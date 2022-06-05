package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.Track
import com.github.unthingable.Util.Timed
import com.github.unthingable.framework.mode.{GateMode, IntActivatedLayer, ModeButtonLayer}
import com.github.unthingable.framework.binding.{Binding, HB, SupColorStateB}
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.Jam

import java.time.Instant

trait MacroL { this: Jam =>
  lazy val macroLayer = new ModeButtonLayer("MACRO", j.macroButton, GateMode.Gate, silent = true) {
    var bumpedStrip  : Option[IntActivatedLayer] = None
    var bumpedSubMode: Option[Int]               = None
    var controlToggleSub: Option[Int] = None // more dirty hacks

    override def activate(): Unit = {
      super.activate()
      // dirty hack to show user controls
      if (j.control.btn.isPressed().get) {
        // CONTROL is already active, just need to toggle
        if (!controlLayer.isUserSelected) {
          controlToggleSub = controlLayer.selected
          controlLayer.selectUser()
        } else controlToggleSub.orElse(Some(0)).foreach(controlLayer.select)
      }
      else {
        bumpedStrip = stripGroup.layers.find(_.isOn)
          .collect { case x: IntActivatedLayer => x }
          .filter(_ != controlLayer)
        if (!controlLayer.isUserSelected) {
          bumpedSubMode = controlLayer.selected
          controlLayer.selectUser()
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
        track.isGroup.markInterested()

        var lastPress: Option[Timed[Track]] = None

        def select(track: Track): Unit = {
          val now = Instant.now()
          if (lastPress.exists(v =>
            v.value == track &&
            v.instant.isAfter(now.minusMillis(200)))
              && track.isGroup.get())
            track.isGroupExpanded.toggle()
          else
            ext.cursorTrack.selectChannel(track)
          lastPress = Some(Timed(track, now))
        }

        Vector(
          SupColorStateB(btn.light, () =>
            if (isSelected.get())
              JamColorState(JamColorBase.WHITE, 3)
            else
              JamColorState(track.color().get(), 0)),
          HB(btn.btn.pressedAction, "direct select track", () => select(track)),
          HB(btn.btn.releasedAction, "direct select release", () => ()),
        )
      }
  }
}
