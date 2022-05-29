package com.github.unthingable.framework.binding

import com.github.unthingable.jam.surface.Button

// event binding
trait HwEvent // hardware events: button presses, etc.
trait ExtEvent // extention events: mode activations, etc.
trait Command
type Event = HwEvent | ExtEvent | Command

//case class SideEffect(f: Event => Unit)

//type Command
case class SideEffect(f: Event => Unit)

type Outcome = Command | SideEffect

enum ButtonEvt extends HwEvent:
  case Press(buttonId: String)
  case Release(buttonId: String)

/* What is the main problem?
The difference between raw button press events and higher order events (combos and chords) is fuzzy.

Main point of events is to decouple from callback "actions" and pass events through our own event processor.
*/
