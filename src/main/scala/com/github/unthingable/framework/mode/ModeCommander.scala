package com.github.unthingable.framework.mode

import com.github.unthingable.framework.binding.*

enum LayerCommand extends ExtEvent:
  case Activate, Deactivate

case class ModeCommand(
  cmd: LayerCommand,
  mode: ModeLayer,
  when: () => Boolean = () => true
)

// sealed trait ModeCommand {
//   def mode: ModeLayer
//   def when: () => Boolean
// }

// OneOf, AllOf

// case class Activate(mode: ModeLayer, when: () => Boolean = () => true) extends ModeCommand
// case class Deactivate(mode: ModeLayer, when: () => Boolean = () => true) extends ModeCommand

trait ModeCommander {
  def eval(c: ModeCommand): Unit
}

object ModeCommander {
  import LayerCommand.*

  def apply(initModes: ModeLayer*): ModeCommander = {

    //
    def index(mode: ModeLayer): Unit = ???

    val ret = new ModeCommander {
      override def eval(c: ModeCommand): Unit =
        if c.when() then
          c.cmd match {
            case Activate   => ???
            case Deactivate => ???
          }
    }

    def activate(mode: ModeLayer): Unit = ret.eval(ModeCommand(Activate, mode))
    def deactivate(mode: ModeLayer): Unit =
      ret.eval(ModeCommand(Deactivate, mode))

    initModes
      .tapEach(index)
    // .tapEach(activate)

    ret
  }
}
