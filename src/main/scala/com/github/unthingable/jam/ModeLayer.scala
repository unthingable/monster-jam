package com.github.unthingable.jam

import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.BindingDSL._
import com.github.unthingable.jam.surface.{Button, FakeAction, OnOffButton}

import java.time.{Duration, Instant}

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

trait ModeLayer {
  val name: String
  // all bindings when layer is active
  def modeBindings: Seq[Binding[_,_,_]]
  implicit val ext: MonsterJamExt

  var isOn: Boolean = false

  // called when layer is activated/deactivated by the container
  def activate(): Unit = isOn = true
  def deactivate(): Unit = isOn = false

  override def hashCode(): Int = name.hashCode
}

/**
 * Layer whose (de)activation is controlled by actions
 */
trait ActivatedLayer[+A <: HBS] {
  val activateAction  : A
  val deactivateAction: A
}

/**
 * (De)activation is triggered by external actions
 */
trait ExtActivatedLayer extends ActivatedLayer[HBS]

/**
 * (De)activation is triggered by internal actions: must invoke them explicitly
 */
trait IntActivatedLayer extends ActivatedLayer[FakeAction] {
  override final val activateAction  : FakeAction = FakeAction()
  override final val deactivateAction: FakeAction = FakeAction()
}

trait ListeningLayer {
  // all bindings when layer is ready and listening
  val loadBindings: Seq[Binding[_,_,_]]
}

// does not self-activate
abstract class SimpleModeLayer(
  val name: String
)(implicit val ext: MonsterJamExt) extends ModeLayer {}

object SimpleModeLayer {
  def apply(name: String, modeBindings: Seq[Binding[_, _, _]])
    (implicit ext: MonsterJamExt): SimpleModeLayer = {
    val x = modeBindings
    new SimpleModeLayer(name) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
    }
  }
}

sealed trait GateMode
object GateMode {
  case object Gate extends GateMode
  case object Toggle extends GateMode
  case object Auto extends GateMode
  case object OneWay extends GateMode
}

abstract class ModeButtonLayer(
  val name: String,
  val modeButton: Button,
  val gateMode: GateMode = GateMode.Auto,
  val silent: Boolean = false
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer with ListeningLayer {
  private var pressedAt: Instant = null

  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    HB(modeButton.pressedAction, s"$name: mode button pressed, isOn: " + isOn, () => {
      pressedAt = Instant.now()
      if (isOn) {
        // this press is only captured when the mode is still active
        if (gateMode != GateMode.OneWay)
          deactivateAction.invoke()
      } else
        activateAction.invoke()
    },
      tracked = false),
    HB(modeButton.releasedAction, s"$name: mode button released", () => gateMode match {
      case GateMode.Gate                     => if (isOn) deactivateAction.invoke()
      case GateMode.Toggle | GateMode.OneWay => ()
      case GateMode.Auto                     =>
        if (isOn) {
          val operated = modeBindings.collect{case x: OutBinding[_,_] => x}.exists(_.wasOperated)
          val elapsed  = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
          if (operated || elapsed)
            deactivateAction.invoke()
        }
    },
      tracked = false)
  ) ++ (modeButton match {
    case b: OnOffButton if !silent => Vector(SupBooleanB(b.light.isOn, () => isOn))
    case _                         => Vector.empty
  })
}

object ModeButtonLayer {
  def apply(name: String, modeButton: OnOffButton, modeBindings: Seq[Binding[_, _, _]],
    gateMode: GateMode = GateMode.Auto,
    silent: Boolean = false)
    (implicit ext: MonsterJamExt): ModeButtonLayer = {
    val x = modeBindings
    new ModeButtonLayer(name, modeButton, gateMode, silent) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
    }
  }
}

abstract class SubModeLayer(
  val name: String
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer

sealed trait CycleMode
object CycleMode {
  case object Cycle extends CycleMode
  case object Select extends CycleMode
  case object GateSelect extends CycleMode
}

abstract class ModeCycleLayer(
  val name: String,
  val modeButton: OnOffButton,
  val cycleMode: CycleMode,
  val silent: Boolean = false,
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer with ListeningLayer {

  val subModes: Seq[ModeLayer with IntActivatedLayer]

  var selected: Option[Int] = Some(0)

  override def activate(): Unit = {
    super.activate()
    selected.foreach(subModes(_).activateAction.invoke())
  }

  override def deactivate(): Unit = {
    selected.foreach(subModes(_).deactivateAction.invoke())
    super.deactivate()
  }

  def cycle(): Unit = {
    selected.map(i => (i + 1) % subModes.length).orElse(Some(0)).foreach(select)
  }

  def select(idx: Int): Unit = {
    if (isOn) selected.foreach(subModes(_).deactivateAction.invoke())
    val mode = subModes(idx)
    ext.host.println((if (isOn) "activating" else "selecting") + s" submode ${mode.name}")
    selected = Some(idx)
    if (isOn) mode.activateAction.invoke()
  }

  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    HB(modeButton.pressedAction, s"$name cycle load MB pressed", () => if (!isOn) activateAction.invoke(), tracked = false)
  ) ++ (if (!silent) Vector(SupBooleanB(modeButton.light.isOn, () => isOn)) else Vector.empty)

  // if overriding, remember to include these
  def modeBindings: Seq[Binding[_, _, _]] = cycleMode match {
    case CycleMode.Cycle      =>
      Vector(
        HB(modeButton.pressedAction, s"$name cycle", () => cycle(), tracked = false, behavior = BindingBehavior(exclusive = false))
      )
    case CycleMode.GateSelect =>
      Vector(
        HB(modeButton.pressedAction, s"$name gate on", () => activateAction.invoke(), tracked = false, behavior = BindingBehavior(exclusive = false)),
        HB(modeButton.releasedAction, s"$name gate off", () => deactivateAction.invoke(), tracked = false, behavior = BindingBehavior(exclusive = false))
      )
    case _                    => Vector.empty
  }
}
