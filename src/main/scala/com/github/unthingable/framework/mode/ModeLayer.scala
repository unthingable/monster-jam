package com.github.unthingable.framework.mode

import com.bitwig.extension.controller.api.OnOffHardwareLight
import com.github.unthingable.framework.binding.BindingDSL.*
import com.github.unthingable.framework.binding.{Binding, ButtonEvt, OutBinding, SupBooleanB, BindingBehavior as BB, ModeCommand}
import com.github.unthingable.jam.surface.{FakeAction, FakeButton, HasButtonState, HasHwButton, HasOnOffLight, JamOnOffButton, WithSource}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.framework.{HasId}

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier
import com.bitwig.`extension`.controller.api.HardwareButton
import com.github.unthingable.jam.surface.HasLight
import com.github.unthingable.jam.surface.JamControl
import com.github.unthingable.framework.binding.EB

/**
 * A group of control bindings to specific host/app functions that plays well with other layers.
 *
 * A mode has two non-dormant states:
 * - ready: not active, but bindings are in place to activate
 * - active: mode layer is on
 *
 * Similarly, there are two sets of bindings:
 * - load bindings: activated when mode is placed in the ready state
 * - active (mode) bindings
 *
 * Bindings activation is managed externally by layer container.
 * @param modeBindings active bindings for this mode
 */

trait ModeLayer extends IntActivatedLayer, HasId {
  // all bindings when layer is active
  def modeBindings: Seq[Binding[_,_,_]]

  def silent: Boolean = false

  var isOn: Boolean = false
  var activeAt: Instant = Instant.now()

  // def ifOn(f: => Unit): () => Unit = () => if (isOn) f

  // called when layer is activated/deactivated by the container
  def onActivate(): Unit = {
    activeAt = Instant.now()
    isOn = true
  }

  def onDeactivate(): Unit = isOn = false

  override def toggleEvent = if (isOn) deactivateEvent else activateEvent

  protected def maybeLightB(b: HasButtonState): Seq[SupBooleanB] =
    val ml = JamControl.maybeLight(b)
    Util.println(s"$id light: $ml $silent")
    ml.filter(_ => !silent).toSeq.map((l: OnOffHardwareLight) => SupBooleanB(l.isOn, () => isOn))
  
  // let's just say we're too lazy to import cats and make a proper Show instance
  override def toString(): String = s"ML:$id"
}

/**
 * Layer whose (de)activation is controlled by actions
 */
trait ActivatedLayer[+A] {
  def activateEvent  : A
  def deactivateEvent: A
  def toggleEvent: A
}

/**
 * (De)activation is triggered by internal actions: must invoke them explicitly
 */
trait IntActivatedLayer extends ActivatedLayer[Seq[ModeCommand[_]]] {
  /* main act/deact events to bind to */
  protected[mode] val selfActivateEvent = WithSource(ModeCommand.Activate(this), this)
  protected[mode] val selfDeactivateEvent = WithSource(ModeCommand.Deactivate(this), this)

  /* */
  def activateEvent: Vector[ModeCommand[_]] = selfActivateEvent.value +: maybeActivate
  def deactivateEvent: Vector[ModeCommand[_]] = maybeDeactivate :+ selfDeactivateEvent.value

  // /* Additional commands to fire after activation and before deactivation - override */
  // def subActivate: Vector[ModeCommand[_]]
  // def subDeactivate: Vector[ModeCommand[_]]

  private inline def maybeActivate: Vector[ModeCommand[?]] = this match
    case l: HasSubModes => l.subModesToActivate.flatMap(_.activateEvent)
    case _ => Vector.empty

  private inline def maybeDeactivate: Vector[ModeCommand[?]] = this match
    case l: HasSubModes => l.subModesToDeactivate.flatMap(_.deactivateEvent)
    case _ => Vector.empty
}

trait ListeningLayer {
  // all bindings when layer is ready and listening
  val loadBindings: Seq[Binding[_,_,_]]
}

// does not self-activate
abstract class SimpleModeLayer(
  val id: String
)(using MonsterJamExt) extends ModeLayer {}

object SimpleModeLayer {
  inline def apply(name: String, modeBindings: Seq[Binding[_, _, _]])
    (using MonsterJamExt): SimpleModeLayer = {
    val x = modeBindings
    new SimpleModeLayer(name) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
      val subActivate: Vector[ModeCommand[_]] = Vector.empty
      val subDeactivate: Vector[ModeCommand[_]] = Vector.empty
    }
  }
}

// case class ModeButton(press: ButtonEvt, release: ButtonEvt, light: Option[OnOffHardwareLight])

// object ModeButton {
//   def apply(b: HasButtonState): ModeButton = ModeButton(
//     b.st.pressedE,
//     b.st.releasedE,
//     b match
//       case x: HasOnOffLight => Some(x.light)
//       case _                => None
//   )
// }

sealed trait GateMode
object GateMode {
  case object Gate extends GateMode
  case object Toggle extends GateMode
  case object Auto extends GateMode
  case object OneWay extends GateMode
}

abstract class ModeButtonLayer(
  val id: String,
  val modeButton: HasButtonState, // & HasLight[_],
  val gateMode: GateMode = GateMode.Auto,
  override val silent: Boolean = false
)(using ext: MonsterJamExt) extends ModeLayer, ListeningLayer {
  private var pressedAt: Instant = null

  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    EB(modeButton.st.press, s"$id: mode button pressed, isOn: " + isOn, () => {
      pressedAt = Instant.now()
      if (isOn) {
        // this press is only captured when the mode is still active
        if (gateMode != GateMode.OneWay)
          deactivateEvent
          // Vector(deactivateEvent.value)
        else
          Vector.empty
      } else
        activateEvent
        // Vector(activateEvent.value)
    }),
    EB(modeButton.st.release, s"$id: mode button released", () => released)
  ) ++ maybeLightB(modeButton)

  // TODO inline
  private def released: Seq[ModeCommand[_]] = gateMode match {
      case GateMode.Gate                     => if (isOn) deactivateEvent else Seq.empty
      case GateMode.Toggle | GateMode.OneWay => Vector.empty
      case GateMode.Auto                     =>
        if (isOn) {
          val operated = modeBindings.collect{case x: OutBinding[_,_,_] => x}.exists(_.operatedAt.nonEmpty)
          val elapsed  = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
          if (operated || elapsed)
            deactivateEvent
            // Vector(deactivateEvent.value)
          else
            Vector.empty
        } else Vector.empty
      }
}

object ModeButtonLayer {
  inline def apply(name: String, modeButton: JamOnOffButton, modeBindings: Seq[Binding[_, _, _]],
    gateMode: GateMode = GateMode.Auto,
    silent: Boolean = false)
    (using MonsterJamExt): ModeButtonLayer = {
    val x = modeBindings
    new ModeButtonLayer(name, modeButton, gateMode, silent) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
      val subActivate: Vector[ModeCommand[_]] = Vector.empty
      val subDeactivate: Vector[ModeCommand[_]] = Vector.empty
    }
  }
}

enum CycleMode:
  // Cycle through each sublayer on modebutton press
  case Cycle
  // No cycling, select sublayers externally
  case Select
  // Active when button held down, no cycling
  case Gate 
  // Like GateSelect, but sticks after a long press
  case Sticky 

trait HasSubModes:
  val subModes: Vector[ModeLayer]
  def subModesToActivate: Vector[ModeLayer]
  def subModesToDeactivate: Vector[ModeLayer]

// duplicates ModeGraph functionality, some day will need a rewrite
abstract class MultiModeLayer(
  val id: String,
)(using ext: MonsterJamExt) extends ModeLayer, HasSubModes {

  // override def onActivate(): Unit = {
  //   super.onActivate()
  // }

  // override def onDeactivate(): Unit = {
  //   subModes.filter(_.isOn).map(_.deactivateEvent).foreach(ext.events.eval(_))
  //   super.onDeactivate()
  // }
}

abstract class ModeCycleLayer(
  override val id: String,
)(using ext: MonsterJamExt) extends MultiModeLayer(id) {
  protected var isStuck: Boolean = false

  var selected: Option[Int] = Some(0)

  override def subModesToActivate: Vector[ModeLayer] = 
    val ret = selected.map(subModes(_)).toVector
    Util.println(s"debug: for $id submode activators are $ret")
    ret

  override def subModesToDeactivate: Vector[ModeLayer] = subModes.filter(_.isOn)

  // override def onActivate(): Unit = {
  //   super.onActivate()
  //   selected.map(subModes(_).activateEvent).foreach(ext.events.eval(_))
  // }

  // override def onDeactivate(): Unit = {
  //   selected.map(subModes(_).deactivateEvent).foreach(ext.events.eval(_))
  //   super.onDeactivate()
  // }

  /** Cycle among all submodes, from currently selected one 
   */
  def cycle(): Unit = {
    selected.map(i => (i + 1) % subModes.size).orElse(Some(0)).foreach(select)
  }

  /** Cycle among a subset of submodes
   */
  def cycle(indices: Int*): Unit =
    selected
    .map(indices.indexOf)
    .map(x => (x + 1) % indices.size)
    .foreach(select)

  def select(idx: Int): Unit = {
    if (isOn && !selected.contains(idx))
      selected.map(subModes(_).deactivateEvent).foreach(ext.events.eval(_*))
      val mode = subModes(idx)
      Util.println("sub: " + (if (isOn) "activating" else "selecting") + s" submode ${mode.id}")
      selected = Some(idx)
      ext.events.eval(mode.activateEvent*)
  }
}

abstract class ModeButtonCycleLayer(
  name: String,
  // val modeButton: OnOffButton with HwButton, // Button[_ <: ButtonActions],
  // val modeButton: OnOffButton with Button[_ <: ButtonActions],
  // val modeButton: JamOnOffButton,
  val modeButton: HasButtonState,
  val cycleMode: CycleMode,
  override val silent: Boolean = false,
  val siblingOperatedModes: Seq[ModeLayer] = Vector(),
)(using ext: MonsterJamExt) extends ModeCycleLayer(name), ListeningLayer {

  def stickyPress: Vector[ModeCommand[_]] = {
    (isOn, cycleMode: CycleMode) match {
      case (false, _) => activateEvent // Vector(activateEvent.value)
      case _ => Vector.empty
    }
  }

  // bindings to inspect when unsticking
  def operatedBindings: Iterable[Binding[_, _, _]] = (selected.map(subModes) ++ siblingOperatedModes).flatMap(_.modeBindings)

  def stickyRelease: Vector[ModeCommand[_]] = {
    (isOn, cycleMode: CycleMode) match {
      case (true, CycleMode.Gate) => deactivateEvent // Vector(deactivateEvent.value)
      case (true, CycleMode.Sticky) =>
        lazy val operated =
          operatedBindings.operatedAfter(activeAt)
          
        if (isStuck || !Instant.now().isAfter(activeAt.plus(Duration.ofMillis(500))) || operated) {
          isStuck = false
          deactivateEvent
          // Vector(deactivateEvent.value)
        } else
          isStuck = true
          Vector.empty
      case _ => Vector.empty
    }
  }

  // overrideable
  def lightOn: BooleanSupplier = () => isOn

  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    EB(modeButton.st.press, s"$name cycle load MB pressed", () => if (!isOn) activateEvent else Seq.empty, BB(tracked = false))
  ) ++ maybeLightB(modeButton) //(if (!silent) Vector(SupBooleanB(modeButton.light.isOn, lightOn)) else Vector.empty)

  // if overriding, remember to include these
  def modeBindings: Seq[Binding[_, _, _]] = cycleMode match {
    case CycleMode.Cycle                   =>
      Vector(
        EB(modeButton.st.press, s"$name cycle", () => cycle(), BB(tracked = false, exclusive = false))
      )
    case CycleMode.Gate | CycleMode.Sticky =>
      Vector(
        EB(modeButton.st.press, s"$name gate on", () => stickyPress, BB(tracked = false, exclusive = false)),
        EB(modeButton.st.release, s"$name gate off", () => stickyRelease, BB(tracked = false, exclusive = false))
      )
    case _                                 => Vector.empty
  }
}
