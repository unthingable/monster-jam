package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{BooleanValue, HardwareActionBindable, Scrollable}
import com.github.unthingable.JamSettings.DpadScroll
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.framework.binding.{Binding, HB, SupBooleanB}
import com.github.unthingable.jam.surface.JamOnOffButton
import com.github.unthingable.jam.Jam

trait Dpad { this: Jam =>
  lazy val dpad = new SimpleModeLayer("dpad") {
    val actionMap = Vector(
      j.dpad.left -> trackBank.canScrollBackwards,
      j.dpad.right -> trackBank.canScrollForwards,
      j.dpad.up -> sceneBank.canScrollBackwards,
      j.dpad.down -> sceneBank.canScrollForwards
    )

    actionMap.foreach { case (b: JamOnOffButton, e: BooleanValue) =>
      e.markInterested()
    }

    def scroll(forward: Boolean, target: Scrollable): HardwareActionBindable = {
      ext.host.createAction(() =>
        (j.Mod.Shift.btn.isPressed ^ (ext.preferences.shiftDpad.get() == DpadScroll.`single/page`), forward) match {
          case (false, true)  => target.scrollPageForwards()
          case (false, false) => target.scrollPageBackwards()
          case (true, true)   => target.scrollForwards()
          case (true, false)  => target.scrollBackwards()
        }, () => s"scroll_$forward")
    }

    override val modeBindings: Seq[Binding[_, _, _]] = Vector(
      HB(j.dpad.left.btn.pressedAction, "page left", scroll(false, trackBank)),
      HB(j.dpad.right.btn.pressedAction, "page right", scroll(true, trackBank)),
      HB(j.dpad.up.btn.pressedAction, "page up", scroll(false, sceneBank)),
      HB(j.dpad.down.btn.pressedAction, "page down", scroll(true, sceneBank)),
    ) ++ actionMap.map { case (b: JamOnOffButton, e: BooleanValue) =>
      SupBooleanB(b.light.isOn, e)
    }
  }
}
