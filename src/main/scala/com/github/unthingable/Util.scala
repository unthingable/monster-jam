package com.github.unthingable

import com.bitwig.extension.controller.api.{ControllerHost, HardwareActionBindable}

trait Util {
  val host: ControllerHost

  def binding(f: () => Unit, id: String): HardwareActionBindable = host.createAction(() => f(), () => id)
}
