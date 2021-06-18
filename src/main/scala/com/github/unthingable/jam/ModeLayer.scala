package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.{BooleanValueChangedCallback, ColorValueChangedCallback, ValueChangedCallback}
import com.bitwig.extension.controller.api._
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.BindingDSL._
import com.github.unthingable.jam.surface.{FakeAction, OnOffButton}

import java.time.{Duration, Instant}
import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.mutable

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
  val modeBindings: Seq[Binding[_,_,_]]
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
  val modeButton: OnOffButton,
  val gateMode: GateMode = GateMode.Auto,
  val silent: Boolean = false
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer with ListeningLayer {
  private var pressedAt: Instant = null

  override final val loadBindings: Seq[Binding[_, _, _]] = Seq(
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
  ) ++ (if (!silent) Seq(SupBooleanB(modeButton.light.isOn, () => isOn)) else Seq())
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

class SubModeLayer(
  val name: String
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer {
  override val modeBindings: Seq[Binding[_, _, _]] = Seq()
}

abstract class ModeCycleLayer(
  val name: String,
  val modeButton: OnOffButton,
  val gateMode: GateMode, // maybe TODO
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer with ListeningLayer {

  val subModes: Seq[SubModeLayer]

  var currentMode: Option[SubModeLayer] = None

  override def activate(): Unit = {
    super.activate()
    if (currentMode.isEmpty) currentMode = subModes.headOption
    currentMode.foreach(_.activateAction.invoke())
  }

  override def deactivate(): Unit = {
    currentMode.foreach(_.deactivateAction.invoke())
    super.deactivate()
  }

  override val loadBindings: Seq[Binding[_, _, _]] = Seq(
    HB(modeButton.pressedAction, s"$name cycle load MB pressed", () => if (!isOn) activateAction.invoke(), tracked = false),
    SupBooleanB(modeButton.light.isOn, () => isOn)
  )

  override val modeBindings: Seq[Binding[_, _, _]] = Seq(
    HB(modeButton.pressedAction, s"$name cycle", () => cycle())
  )

  def cycle(): Unit = {
    currentMode.foreach(_.deactivateAction.invoke())
    currentMode = currentMode match {
      case Some(l) =>
        val idx = subModes.indexOf(l, 0)
        Some(subModes((idx + 1) % subModes.length))
      case None => subModes.headOption
    }
    ext.host.println(s"activating submode ${currentMode.get.name}")
    currentMode.foreach(_.activateAction.invoke())
  }
}
