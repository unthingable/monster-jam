package com.github.unthingable.framework.mode

import com.bitwig.`extension`.controller.api.HardwareButton
import com.bitwig.extension.controller.api.OnOffHardwareLight
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.HasId
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.BindingDSL.*
import com.github.unthingable.framework.binding.ButtonEvt
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.ModeCommand
import com.github.unthingable.framework.binding.OutBinding
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.jam.surface.FakeAction
import com.github.unthingable.jam.surface.FakeButton
import com.github.unthingable.jam.surface.HasButtonState
import com.github.unthingable.jam.surface.HasHwButton
import com.github.unthingable.jam.surface.HasLight
import com.github.unthingable.jam.surface.HasOnOffLight
import com.github.unthingable.jam.surface.JamControl
import com.github.unthingable.jam.surface.JamOnOffButton
import com.github.unthingable.jam.surface.WithSource

import java.time.Duration
import java.time.Instant
import java.util.function.BooleanSupplier
import scala.concurrent.duration.FiniteDuration

enum ModeState derives CanEqual:
  case Inactive, Activating, Active, Deactivating

/** A group of control bindings to specific host/app functions that plays well with other layers.
  *
  * A mode has two non-dormant states:
  *   - ready: not active, but bindings are in place to activate
  *   - active: mode layer is on
  *
  * Similarly, there are two sets of bindings:
  *   - load bindings: activated when mode is placed in the ready state
  *   - active (mode) bindings
  *
  * Bindings activation is managed externally by layer container.
  * @param modeBindings
  *   active bindings for this mode
  */

trait ModeLayer extends IntActivatedLayer, HasId derives CanEqual:
  // all bindings when layer is active
  def modeBindings: Seq[Binding[?, ?, ?]]

  def silent: Boolean = false

  protected var activeAt: Option[Instant] = None

  import ModeState.*
  protected var modeState = (Inactive, Instant.now())

  /** Current mode state, set externally by ModeGraph, read by mode implementations. For when mode needs to know when
    * it's being activated or shut down.
    *
    * This extends and somewhat duplicates activeAt inspection, that may be removed later.
    */
  def setModeState(st: ModeState) = modeState = (st, Instant.now())

  final inline def isOn: Boolean = activeAt.isDefined

  final private inline def dirtyBindings(
    withBindings: Binding[?, ?, ?]*
  ): Seq[OutBinding[?, ?, ?]] =
    activeAt.toSeq.flatMap(withBindings.operatedAfter)

  // because activeAt is hidden now
  final inline def hasDirtyBindings(withBindings: Binding[?, ?, ?]*): Boolean =
    activeAt.map(withBindings.hasOperatedAfter).getOrElse(false)

  final inline def isOlderThan(inline duration: FiniteDuration): Boolean =
    isOlderThan(Duration.ofNanos(duration.toNanos))

  final inline def isOlderThan(inline duration: Duration): Boolean =
    activeAt.exists(act => Instant.now().isAfter(act.plus(duration)))

  final inline def isOlderThan(inline instant: Instant): Boolean =
    activeAt.exists(act => instant.isAfter(act))

  /** Override if mode needs to consider additional bindings when making deactivation decisions */
  def extraOperated: Iterable[Binding[?, ?, ?]] = Seq.empty

  // called when layer is activated/deactivated by the container
  def onActivate(): Unit = activeAt = activeAt.orElse(Some(Instant.now()))

  def onDeactivate(): Unit = activeAt = None

  override def toggleEvent = if isOn then deactivateEvent else activateEvent

  protected def maybeLightB(b: HasButtonState): Seq[SupBooleanB] =
    val ml = JamControl.maybeLight(b)
    Util.println(s"$id light: $ml $silent")
    ml.filter(_ => !silent).toSeq.map((l: OnOffHardwareLight) => SupBooleanB(l, () => isOn))

  // let's just say we're too lazy to import cats and make a proper Show instance
  override def toString(): String = s"ML:$id"
end ModeLayer

/** Layer whose (de)activation is controlled by actions
  */
trait ActivatedLayer[+A]:
  def activateEvent: A
  def deactivateEvent: A
  def toggleEvent: A

/** (De)activation is triggered by internal actions: must invoke them explicitly
  */
trait IntActivatedLayer extends ActivatedLayer[Seq[ModeCommand[?]]]:
  /* main act/deact events to bind to */
  protected[mode] val selfActivateEvent   = WithSource(ModeCommand.Activate(this), this)
  protected[mode] val selfDeactivateEvent = WithSource(ModeCommand.Deactivate(this), this)

  /* */
  def activateEvent: Vector[ModeCommand[?]]   = selfActivateEvent.value +: maybeActivate
  def deactivateEvent: Vector[ModeCommand[?]] = maybeDeactivate :+ selfDeactivateEvent.value

  private inline def maybeActivate: Vector[ModeCommand[?]] = this match
    case l: HasSubModes => l.subModesToActivate.flatMap(_.activateEvent)
    case _              => Vector.empty

  private inline def maybeDeactivate: Vector[ModeCommand[?]] = this match
    case l: HasSubModes => l.subModesToDeactivate.flatMap(_.deactivateEvent)
    case _              => Vector.empty

trait ListeningLayer:
  // all bindings when layer is ready and listening
  val loadBindings: Seq[Binding[?, ?, ?]]

// does not self-activate
abstract class SimpleModeLayer(
  val id: String
)(using MonsterJamExt)
    extends ModeLayer {}

object SimpleModeLayer:
  inline def apply(name: String, modeBindings: Seq[Binding[?, ?, ?]])(using
    MonsterJamExt
  ): SimpleModeLayer =
    val x = modeBindings
    new SimpleModeLayer(name):
      override val modeBindings: Seq[Binding[?, ?, ?]] = x

enum GateMode derives CanEqual:
  case Gate, // active only when mode button is pressed
    Toggle,
    Auto,        // toggle on momentary press, gate when held
    OneWay,      // pressing turns mode on
    AutoInverse, // toggle on momentary press, ignore release after long press
    SmartRelease // toggle momentary, momentary when operated

trait ModeButtonLayer(
  val id: String,
  val modeButton: HasButtonState,
  val gateMode: GateMode = GateMode.Auto,
  override val silent: Boolean = false
)(using ext: MonsterJamExt)
    extends ModeLayer,
      ListeningLayer:
  import GateMode.*
  private var pressedAt: Instant = null
  private object NR:
    // workaround for java.lang.AbstractMethodError: Receiver class com.github.unthingable.jam.Jam$$anon$4 does not define or inherit an implementation of the resolved method 'abstract void noRelease_$eq(boolean)' of interface com.github.unthingable.framework.mode.ModeButtonLayer.
    var noRelease: Boolean = true
  import NR.*

  override val loadBindings: Seq[Binding[?, ?, ?]] = Vector(
    EB(
      modeButton.st.press,
      s"$id: mode button pressed",
      () =>
        Util.println(" isOn: " + isOn)
        pressedAt = Instant.now()
        gateMode match
          case Auto | Gate =>
            noRelease = false
            if !isOn then activateEvent else Vector.empty
          case AutoInverse =>
            noRelease = !isOn
            if !isOn then activateEvent else Vector.empty
          case Toggle =>
            noRelease = true
            if !isOn then activateEvent else deactivateEvent
          case OneWay =>
            noRelease = true
            if !isOn then activateEvent else Vector.empty
          case SmartRelease =>
            noRelease = false
            Vector.empty
      , // bb = BB(exclusive = false)
    ),
    EB(modeButton.st.release, s"$id: mode button released", () => released)
  ) ++ maybeLightB(modeButton)

  private def released: Seq[ModeCommand[?]] =
    inline def isHeldLongEnough = Instant.now().isAfter(pressedAt.plus(Duration.ofMillis(500)))
    inline def operated         = (modeBindings.view ++ extraOperated.view).hasOperatedAfter(pressedAt)
    inline def isAlreadyOn      = activeAt.map(pressedAt.isAfter).getOrElse(false)

    if noRelease then Vector.empty
    else
      gateMode match
        case Gate if isOn => deactivateEvent
        case Auto if isOn =>
          if isHeldLongEnough || isAlreadyOn || operated then deactivateEvent
          else Vector.empty
        case AutoInverse if isOn =>
          if isHeldLongEnough || operated then Vector.empty else deactivateEvent
        case SmartRelease =>
          if isHeldLongEnough || operated then Vector.empty else toggleEvent
        case _ => Vector.empty
end ModeButtonLayer

object ModeButtonLayer:
  inline def apply(
    name: String,
    modeButton: HasButtonState,
    modeBindings: Seq[Binding[?, ?, ?]],
    gateMode: GateMode = GateMode.Auto,
    silent: Boolean = false
  )(using MonsterJamExt): ModeButtonLayer =
    val x = modeBindings
    new ModeButtonLayer(name, modeButton, gateMode, silent):
      override val modeBindings: Seq[Binding[?, ?, ?]] = x
      val subActivate: Vector[ModeCommand[?]]          = Vector.empty
      val subDeactivate: Vector[ModeCommand[?]]        = Vector.empty

enum CycleMode derives CanEqual:
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
trait MultiModeLayer(using ext: MonsterJamExt) extends ModeLayer, HasSubModes:
  override def subModesToActivate: Vector[ModeLayer]   = subModes.filter(_.isOn)
  override def subModesToDeactivate: Vector[ModeLayer] = subModes.filter(_.isOn)

object MultiModeLayer:
  def apply(_id: String, _subModes: Iterable[ModeLayer])(using MonsterJamExt): MultiModeLayer = new MultiModeLayer:
    override def id: String = _id

    override def modeBindings: Seq[Binding[?, ?, ?]] = Seq.empty

    override val subModes: Vector[ModeLayer] = _subModes.toVector

abstract class ModeCycleLayer(
  override val id: String,
)(using ext: MonsterJamExt)
    extends MultiModeLayer:
  protected var isStuck: Boolean = false

  var selected: Option[Int] = Some(0)

  override def subModesToActivate: Vector[ModeLayer] =
    val ret = (selected.map(subModes(_)).toVector ++ super.subModesToActivate // in case they were bumped
    ).distinct
    Util.println(s"debug: for $id submode activators are $ret")
    ret

  /** Cycle among all submodes, from currently selected one
    */
  def cycle(): Unit =
    selected.map(i => (i + 1) % subModes.size).orElse(Some(0)).foreach(select(_))

  /** Cycle among a subset of submodes
    */
  def cycle(indices: Int*): Unit =
    selected
      .map(indices.indexOf)
      .map(x => (x + 1) % indices.size)
      .foreach(select(_))

  // an attempt at multiselect, for a later day
  // def select(idx: Int*): Unit =
  //   if (isOn && idx.nonEmpty)
  //     val toDeactivate = subModes.indices.filter(!idx.contains(_))
  //     ext.events.eval(s"$id select deact")(toDeactivate.map(subModes(_)).filter(_.isOn).flatMap(_.deactivateEvent)*)
  //     ext.events.eval(s"$id select act")(idx.flatMap(subModes(_).activateEvent)*)
  //     selected = idx.lastOption

  def select(idx: Int): Unit =
    if isOn && !selected.contains(idx) then
      ext.events.eval(s"$id select deact")(
        selected
          // .filter(_ != idx)
          .map(subModes(_))
          .filter(_.isOn)
          .toSeq
          .flatMap(_.deactivateEvent)*
      )
    val mode = subModes(idx)
    Util.println("sub: " + (if isOn then "activating" else "selecting") + s" submode ${mode.id}")
    selected = Some(idx)
    if isOn && !mode.isOn then ext.events.eval(s"$id select act")(mode.activateEvent*)
end ModeCycleLayer

abstract class ModeButtonCycleLayer(
  override val id: String,
  override val modeButton: HasButtonState,
  val cycleMode: CycleMode,
  override val gateMode: GateMode = GateMode.OneWay,
  override val silent: Boolean = false,
  val siblingOperatedModes: Seq[ModeLayer] = Vector(),
)(using ext: MonsterJamExt)
    extends ModeCycleLayer(id),
      ModeButtonLayer(id, modeButton):

  def stickyPress: Vector[ModeCommand[?]] =
    (isOn, cycleMode: CycleMode) match
      case (false, _) => activateEvent
      case _          => Vector.empty

  // bindings to inspect when unsticking
  def operatedBindings: Iterable[Binding[?, ?, ?]] = (selected.map(subModes) ++ siblingOperatedModes)
    .flatMap(_.modeBindings) ++ extraOperated

  def stickyRelease: Vector[ModeCommand[?]] =
    (isOn, cycleMode: CycleMode) match
      case (true, CycleMode.Gate) => deactivateEvent
      case (true, CycleMode.Sticky) =>
        lazy val operated = hasDirtyBindings(operatedBindings.toSeq*)

        if isStuck || !isOlderThan(Duration.ofMillis(500)) || operated then
          isStuck = false
          deactivateEvent
        else
          isStuck = true
          Vector.empty
      case _ => Vector.empty

  // overrideable
  def lightOn: BooleanSupplier = () => isOn

  // if overriding, remember to include these
  def modeBindings: Seq[Binding[?, ?, ?]] = cycleMode match
    case CycleMode.Cycle =>
      Vector(
        EB(modeButton.st.press, s"$id cycle", () => cycle(), BB.omni)
      )
    case CycleMode.Gate | CycleMode.Sticky =>
      Vector(
        EB(
          modeButton.st.press,
          s"$id gate on",
          () => stickyPress,
          BB.omni
        ),
        EB(
          modeButton.st.release,
          s"$id gate off",
          () => stickyRelease,
          BB.omni
        )
      )
    case _ => Vector.empty
end ModeButtonCycleLayer
