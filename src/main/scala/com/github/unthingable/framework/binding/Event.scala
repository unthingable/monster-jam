package com.github.unthingable.framework.binding

type Event   = HwEvent | ExtEvent | Command
type Outcome = Command | SideEffect | CmdEffect

trait HwEvent  // hardware events: button presses, etc.
trait ExtEvent // extention events: mode activations, etc.
trait Command

case class SideEffect(f: Event => Unit):
  override def toString: String = ""

case class CmdEffect(f: Event => Seq[Command]):
  override def toString: String = ""

enum ButtonEvt extends HwEvent:
  case Press(buttonId: String)
  case Release(buttonId: String)

enum ModeCommand[+A] extends Command:
  case Activate(obj: A)
  case Deactivate(obj: A)
  // case Toggle(obj: A)

// object Noop extends Command

/* What is the main problem?
The difference between raw button press events and higher order events (combos and chords) is fuzzy.

Main point of events is to decouple from callback "actions" and pass events through our own event processor.
 */

object GlobalEvent:
  case class ClipSelected(globalTrack: Int, globalClip: Int) extends ExtEvent
