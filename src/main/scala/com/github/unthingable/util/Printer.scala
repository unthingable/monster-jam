package com.github.unthingable.util

import java.awt.event.ActionEvent

class Printer(printFun: String => Unit):
  import javax.swing.Timer

  val timer = new Timer(100, (_: ActionEvent) => printFun(""))

  timer.setRepeats(false)

  def println(s: String): Unit =
    if timer.isRunning then timer.stop()
    printFun(s)
    timer.restart()
