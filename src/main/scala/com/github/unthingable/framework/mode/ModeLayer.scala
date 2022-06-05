package com.github.unthingable.framework.mode

import com.bitwig.extension.controller.api.OnOffHardwareLight
import com.github.unthingable.framework.binding.BindingDSL.*
import com.github.unthingable.framework.binding.{Binding, ButtonEvt, HB, OutBinding, SupBooleanB, BindingBehavior as BB, ModeCommand}
import com.github.unthingable.jam.surface.{FakeAction, FakeButton, HasButtonState, HasHwButton, HasOnOffLight, JamOnOffButton}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.framework.{HasId}

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier
import com.bitwig.`extension`.controller.api.HardwareButton
import com.github.unthingable.jam.surface.HasLight

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
  // given ext: MonsterJamExt

  def silent: Boolean = false

  var isOn: Boolean = false
  var activeAt: Instant = Instant.now()

  def ifOn(f: => Unit): () => Unit = () => if (isOn) f

  // called when layer is activated/deactivated by the container
  def onActivate(): Unit = {
    activeAt = Instant.now()
    isOn = true
  }

  def onDeactivate(): Unit = isOn = false

  override def toggleEvent = if (isOn) deactivateEvent else activateEvent

  override def hashCode(): Int = id.hashCode

  protected inline def maybeLight(b: HasButtonState): Option[OnOffHardwareLight] = 
    inline b match 
      case x: HasOnOffLight => Some(x.light)
      case _ => None
  
  protected inline def maybeLightB(b: HasButtonState): Seq[SupBooleanB] =
    val ml = maybeLight(b)
    Util.println(s"$id light: $ml $silent")
    maybeLight(b).filter(_ => !silent).toSeq.map(l => SupBooleanB(l.isOn, () => isOn))
  
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
trait IntActivatedLayer extends ActivatedLayer[ModeCommand[IntActivatedLayer]] {
  /* FIXME: actions are bindable and are called before (de)activate().
  However, the call to (de)activate() is wrapped in another action and wired in ModeGraph, can be confusing.
  */
  override final val activateEvent = ModeCommand.Activate(this)
  override final val deactivateEvent = ModeCommand.Deactivate(this)
}

trait ListeningLayer {
  // all bindings when layer is ready and listening
  val loadBindings: Seq[Binding[_,_,_]]
}

// does not self-activate
abstract class SimpleModeLayer(
  val id: String
)(implicit val ext: MonsterJamExt) extends ModeLayer {}

object SimpleModeLayer {
  def apply(name: String, modeBindings: Seq[Binding[_, _, _]])
    (using MonsterJamExt): SimpleModeLayer = {
    val x = modeBindings
    new SimpleModeLayer(name) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
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
  val modeButton: HasButtonState & HasLight[_],
  val gateMode: GateMode = GateMode.Auto,
  override val silent: Boolean = false
)(using ext: MonsterJamExt) extends ModeLayer, ListeningLayer {
  private var pressedAt: Instant = null

  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    HB(modeButton.st.pressedE, s"$id: mode button pressed, isOn: " + isOn, () => {
      pressedAt = Instant.now()
      if (isOn) {
        // this press is only captured when the mode is still active
        if (gateMode != GateMode.OneWay)
          ext.events.eval(deactivateEvent)
      } else
        ext.events.eval(activateEvent)
    }),
    HB(modeButton.st.releasedE, s"$id: mode button released", () => released)
  ) ++ maybeLightB(modeButton)

  // TODO inline
  private def released = gateMode match {
      case GateMode.Gate                     => if (isOn) ext.events.eval(deactivateEvent)
      case GateMode.Toggle | GateMode.OneWay => ()
      case GateMode.Auto                     =>
        if (isOn) {
          val operated = modeBindings.collect{case x: OutBinding[_,_,_] => x}.exists(_.operatedAt.nonEmpty)
          val elapsed  = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
          if (operated || elapsed)
            ext.events.eval(deactivateEvent)
        }
      }
}

object ModeButtonLayer {
  def apply(name: String, modeButton: JamOnOffButton, modeBindings: Seq[Binding[_, _, _]],
    gateMode: GateMode = GateMode.Auto,
    silent: Boolean = false)
    (using MonsterJamExt): ModeButtonLayer = {
    val x = modeBindings
    new ModeButtonLayer(name, modeButton, gateMode, silent) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
    }
  }
}

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

// duplicates ModeGraph functionality, some day will need a rewrite
abstract class MultiModeLayer(
  val id: String,
)(using ext: MonsterJamExt) extends ModeLayer {
  val subModes: Seq[ModeLayer]

  // override def onActivate(): Unit = {
  //   super.onActivate()
  // }

  override def onDeactivate(): Unit = {
    subModes.filter(_.isOn).map(_.deactivateEvent).foreach(ext.events.eval)
    super.onDeactivate()
  }
}

abstract class ModeCycleLayer(
  override val id: String,
)(using ext: MonsterJamExt) extends MultiModeLayer(id) {
  protected var isStuck: Boolean = false

  var selected: Option[Int] = Some(0)

  override def onActivate(): Unit = {
    super.onActivate()
    selected.map(subModes(_).activateEvent).foreach(ext.events.eval)
  }

  override def onDeactivate(): Unit = {
    selected.map(subModes(_).deactivateEvent).foreach(ext.events.eval)
    super.onDeactivate()
  }

  def cycle(): Unit = {
    selected.map(i => (i + 1) % subModes.length).orElse(Some(0)).foreach(select)
  }

  def select(idx: Int): Unit = {
    if (isOn) selected.map(subModes(_).deactivateEvent).foreach(ext.events.eval)
    val mode = subModes(idx)
    Util.println("sub: " + (if (isOn) "activating" else "selecting") + s" submode ${mode.id}")
    selected = Some(idx)
    if (isOn) ext.events.eval(mode.activateEvent)
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

  def stickyPress(): Unit = {
    (isOn, cycleMode: CycleMode) match {
      case (false, _) => ext.events.eval(activateEvent)
      case _ => ()
    }
  }

  // bindings to inspect when unsticking
  def operatedBindings: Iterable[Binding[_, _, _]] = (selected.map(subModes) ++ siblingOperatedModes).flatMap(_.modeBindings)

  def stickyRelease(): Unit = {
    (isOn, cycleMode: CycleMode) match {
      case (true, CycleMode.Gate) => ext.events.eval(deactivateEvent)
      case (true, CycleMode.Sticky) =>
        lazy val operated =
          operatedBindings.operatedAfter(activeAt)
          
        if (isStuck || !Instant.now().isAfter(activeAt.plus(Duration.ofMillis(500))) || operated) {
          ext.events.eval(deactivateEvent)
          isStuck = false
        } else
          isStuck = true
      case _ => ()
    }
  }

  // overrideable
  def lightOn: BooleanSupplier = () => isOn

  // import reflect.Selectable.reflectiveSelectable
  override final val loadBindings: Seq[Binding[_, _, _]] = Vector(
    HB(modeButton.st.pressedE, s"$name cycle load MB pressed", () => if (!isOn) ext.events.eval(activateEvent), BB(tracked = false))
  ) ++ maybeLightB(modeButton) //(if (!silent) Vector(SupBooleanB(modeButton.light.isOn, lightOn)) else Vector.empty)

  // if overriding, remember to include these
  def modeBindings: Seq[Binding[_, _, _]] = cycleMode match {
    case CycleMode.Cycle                   =>
      Vector(
        HB(modeButton.st.pressedE, s"$name cycle", () => cycle(), BB(tracked = false, exclusive = false))
      )
    case CycleMode.Gate | CycleMode.Sticky =>
      Vector(
        HB(modeButton.st.pressedE, s"$name gate on", () => stickyPress(), BB(tracked = false, exclusive = false)),
        HB(modeButton.st.releasedE, s"$name gate off", () => stickyRelease(), BB(tracked = false, exclusive = false))
      )
    case _                                 => Vector.empty
  }
}
