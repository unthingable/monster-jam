package com.github.unthingable.framework.mode

sealed trait ModeCommand {
  def mode: ModeLayer
  def when: () => Boolean
}

// OneOf, AllOf

case class Activate(mode: ModeLayer, when: () => Boolean = () => true) extends ModeCommand
case class Deactivate(mode: ModeLayer, when: () => Boolean = () => true) extends ModeCommand

trait ModeCommander {
  def eval(c: ModeCommand): Unit
}

object ModeCommander {
  def apply(initModes: ModeLayer*): ModeCommander = {

    //
    def index(mode: ModeLayer): Unit = ???

    val ret = new ModeCommander {
      override def eval(c: ModeCommand): Unit = c match {
        case Activate(m, b) if b() => ???
        case Deactivate(m, b) if b() => ???
      }
    }

    def activate(mode: ModeLayer): Unit = ret.eval(Activate(mode))
    def deactivate(mode: ModeLayer): Unit = ret.eval(Deactivate(mode))

    initModes
      .tapEach(index)
      .tapEach(activate)

    ret
  }
}
