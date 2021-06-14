package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{ControllerHost, HardwareActionBindable}

trait Util {
  implicit class SeqOps[A, S[B] <: Seq[B]](seq: S[A]) {
    def forindex(f: (A, Int) => Unit): S[A] = {
      (0 to seq.length).foreach(idx => f(seq(idx), _))
      seq
    }
  }

  def toColor(color: java.awt.Color): Color =
    Color.fromRGBA(color.getRed, color.getGreen, color.getBlue, color.getAlpha)
}

object Util {
  var println: String => Unit = null
}
