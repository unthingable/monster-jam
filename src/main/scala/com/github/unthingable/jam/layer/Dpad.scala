package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{BooleanValue, HardwareActionBindable, Scrollable}
import com.github.unthingable.jam.surface.JamOnOffButton
import com.github.unthingable.jam.{Binding, HB, Jam, SimpleModeLayer, SupBooleanB}
import com.github.unthingable.JamSettings.DpadScroll

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
        (j.Modifiers.Shift.isPressed() ^ (ext.preferences.shiftDpad.get() == DpadScroll.RegularOne), forward) match {
          case (false, true)  => target.scrollPageForwards()
          case (false, false) => target.scrollPageBackwards()
          case (true, true)   => target.scrollForwards()
          case (true, false)  => target.scrollBackwards()
        }, () => s"scroll_$forward")
    }

    override val modeBindings: Seq[Binding[_, _, _]] = Vector(
      HB(j.dpad.left.pressedAction, "page left", scroll(false, trackBank)),
      HB(j.dpad.right.pressedAction, "page right", scroll(true, trackBank)),
      HB(j.dpad.up.pressedAction, "page up", scroll(false, sceneBank)),
      HB(j.dpad.down.pressedAction, "page down", scroll(true, sceneBank)),
    ) ++ actionMap.map { case (b: JamOnOffButton, e: BooleanValue) =>
      SupBooleanB(b.light.isOn, e)
    }
  }
}
