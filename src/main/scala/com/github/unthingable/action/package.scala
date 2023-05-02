package com.github.unthingable

import com.bitwig.extension.controller.api.ControllerHost

package object action:

  def renameDevices(host: ControllerHost): Unit =
    val trackBank = host.createTrackBank(128, 0, 0)

    (0 until trackBank.getCapacityOfBank).map(trackBank.getItemAt)

  def createActions(host: ControllerHost): Unit =
    host.createAction(() => renameDevices(host), () => "Give devices unique names")
