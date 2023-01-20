package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.BooleanValue
import com.bitwig.extension.controller.api.HardwareActionBindable
import com.bitwig.extension.controller.api.Scrollable
import com.github.unthingable.JamSettings.DpadScroll
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.JamOnOffButton

trait Dpad:
  this: Jam =>
  lazy val dpad = new SimpleModeLayer("dpad"):
    val actionMap = Vector(
      j.dpad.left  -> trackBank.canScrollBackwards,
      j.dpad.right -> trackBank.canScrollForwards,
      j.dpad.up    -> sceneBank.canScrollBackwards,
      j.dpad.down  -> sceneBank.canScrollForwards
    )

    actionMap.foreach {
      case (b: JamOnOffButton, e: BooleanValue) =>
        e.markInterested()
    }

    def scroll(forward: Boolean, target: Scrollable): () => Unit =
      () =>
        (j.Mod.Shift.st.isPressed ^ (ext.preferences.shiftDpad.get() == DpadScroll.`single/page`), forward) match
          case (false, true)  => target.scrollPageForwards()
          case (false, false) => target.scrollPageBackwards()
          case (true, true)   => target.scrollForwards()
          case (true, false)  => target.scrollBackwards()

    override val modeBindings: Seq[Binding[?, ?, ?]] = Vector(
      EB(j.dpad.left.st.press, "page left", scroll(false, trackBank)),
      EB(j.dpad.right.st.press, "page right", scroll(true, trackBank)),
      EB(j.dpad.up.st.press, "page up", scroll(false, sceneBank)),
      EB(j.dpad.down.st.press, "page down", scroll(true, sceneBank)),
    ) ++ actionMap.map {
      case (b: JamOnOffButton, e: BooleanValue) =>
        SupBooleanB(b.light, e)
    }
end Dpad
