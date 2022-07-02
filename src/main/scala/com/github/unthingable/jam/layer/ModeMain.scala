package com.github.unthingable.jam.layer

import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam

trait ModeMain { this: Jam =>
  lazy val modeMain = SimpleModeLayer("modeMain",
    Vector(
      // SONG -> superscene toggle
      // LEVEL -> level select and toggle
      // MACRO -> user bank
      // MACRO -> track selector hold
      // SHIFT -> shift transport, page selectors -- if appropriate?
    )
  )
}
