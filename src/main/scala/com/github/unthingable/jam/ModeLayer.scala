package com.github.unthingable.jam

import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.BindingDSL._
import com.github.unthingable.jam.surface.{Button, FakeAction, OnOffButton}

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier

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
  var activeAt: Instant = Instant.now()

  // called when layer is activated/deactivated by the container
  def activate(): Unit = {
    activeAt = Instant.now()
    isOn = true
  }

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
      tracked = true),
    HB(modeButton.releasedAction, s"$name: mode button released", () => gateMode match {
      case GateMode.Gate                     => if (isOn) deactivateAction.invoke()
      case GateMode.Toggle | GateMode.OneWay => ()
      case GateMode.Auto                     =>
        if (isOn) {
          val operated = modeBindings.collect{case x: OutBinding[_,_] => x}.exists(_.operatedAt.nonEmpty)
          val elapsed  = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
          if (operated || elapsed)
            deactivateAction.invoke()
        }
    },
      tracked = true)
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
  // Cycle through each sublayer on modebutton press
  case object Cycle extends CycleMode
  // No cycling, select sublayers externally
  case object Select extends CycleMode
  // Active when button held down, no cycling
  case object Gate extends CycleMode
  // Like GateSelect, but sticks after a long press
  case object Sticky extends CycleMode
}

abstract class ModeCycleLayer(
  val name: String,
  val modeButton: OnOffButton,
  val cycleMode: CycleMode,
  val silent: Boolean = false,
  val siblingOperatedModes: Seq[ModeLayer] = Vector(),
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer with ListeningLayer {
  private var isStuck: Boolean = false

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
    Util.println("sub: " + (if (isOn) "activating" else "selecting") + s" submode ${mode.name}")
    selected = Some(idx)
    if (isOn) mode.activateAction.invoke()
  }

  def stickyPress(): Unit = {
    (isOn, cycleMode: CycleMode) match {
      case (false, _) => activateAction.invoke()
      case _ => ()
    }
  }

  // bindings to inspect when unsticking
  def operatedBindings: Iterable[Binding[_, _, _]] = (selected.map(subModes) ++ siblingOperatedModes).flatMap(_.modeBindings)

  def stickyRelease(): Unit = {
    (isOn, cycleMode: CycleMode) match {
      case (true, CycleMode.Gate) => deactivateAction.invoke()
      case (true, CycleMode.Sticky) =>
        lazy val operated =
          operatedBindings.outBindings.exists(_.operatedAt.exists(_.isAfter(activeAt)))

        if (isStuck || !Instant.now().isAfter(activeAt.plus(Duration.ofMillis(500))) || operated) {
          deactivateAction.invoke()
          isStuck = false
        } else
          isStuck = true
      case _ => ()
    }
  }

  // overrideable
  def lightOn: BooleanSupplier = () => isOn

  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    HB(modeButton.pressedAction, s"$name cycle load MB pressed", () => if (!isOn) activateAction.invoke(), tracked = false)
  ) ++ (if (!silent) Vector(SupBooleanB(modeButton.light.isOn, lightOn)) else Vector.empty)

  // if overriding, remember to include these
  def modeBindings: Seq[Binding[_, _, _]] = cycleMode match {
    case CycleMode.Cycle                   =>
      Vector(
        HB(modeButton.pressedAction, s"$name cycle", () => cycle(), tracked = false, behavior = BindingBehavior(exclusive = false))
      )
    case CycleMode.Gate | CycleMode.Sticky =>
      Vector(
        HB(modeButton.pressedAction, s"$name gate on", () => stickyPress(), tracked = false, behavior = BindingBehavior(exclusive = false)),
        HB(modeButton.releasedAction, s"$name gate off", () => stickyRelease(), tracked = false, behavior = BindingBehavior(exclusive = false))
      )
    case _                                 => Vector.empty
  }
}
