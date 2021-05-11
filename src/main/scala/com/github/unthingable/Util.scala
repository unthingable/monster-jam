package com.github.unthingable

import com.bitwig.extension.controller.api.{ControllerHost, HardwareActionBindable}

trait Util {
  implicit class SeqOps[A, S[B] <: Seq[B]](seq: S[A]) {
    def forIndex(f: (A, Int) => Unit): S[A] = {
      (0 to seq.length).foreach(idx => f(seq(idx), _))
      seq
    }
  }
}
