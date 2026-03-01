package com.github.unthingable.jam.layer

import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.jam.Jam

trait SelectL:
  this: Jam =>

  lazy val selectNav = new ModeButtonLayer("selectNav", j.select, GateMode.Gate):
    override val modeBindings = Vector(
      EB(j.left.st.press, "previous project", () => ext.application.previousProject()),
      EB(j.right.st.press, "next project", () => ext.application.nextProject()),
      SupBooleanB(j.left.light, () => true),
      SupBooleanB(j.right.light, () => true),
    )
