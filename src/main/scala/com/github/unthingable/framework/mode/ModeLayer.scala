package com.github.unthingable.framework.mode

import com.bitwig.extension.controller.api.OnOffHardwareLight
import com.github.unthingable.framework.binding.BindingDSL.*
import com.github.unthingable.framework.binding.{
  Binding,
  BindingBehavior as BB,
  ButtonEvt,
  ModeCommand,
  OutBinding,
  SupBooleanB
}
import com.github.unthingable.jam.surface.{
  FakeAction,
  FakeButton,
  HasButtonState,
  HasHwButton,
  HasOnOffLight,
  JamOnOffButton,
  WithSource
}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.framework.HasId

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier
import com.bitwig.`extension`.controller.api.HardwareButton
import com.github.unthingable.jam.surface.HasLight
import com.github.unthingable.jam.surface.JamControl
import com.github.unthingable.framework.binding.EB

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

trait ModeLayer extends IntActivatedLayer, HasId {
  // all bindings when layer is active
  def modeBindings: Seq[Binding[_, _, _]]

  def silent: Boolean = false

  protected var activeAt: Option[Instant] = None

  final inline def isOn: Boolean = activeAt.isDefined

  final private inline def dirtyBindings(
    withBindings: Binding[_, _, _]*
  ): Seq[OutBinding[_, _, _]] =
    activeAt.toSeq.flatMap(withBindings.operatedAfter)

  // because activeAt is hidden now
  final inline def hasDirtyBindings(withBindings: Binding[_, _, _]*): Boolean =
    activeAt.map(withBindings.hasOperatedAfter).getOrElse(false)

  final inline def isOlderThan(inline duration: Duration): Boolean =
    activeAt.exists(act => Instant.now().isAfter(act.plus(duration)))

  final inline def isOlderThan(inline instant: Instant): Boolean =
    activeAt.exists(act => instant.isAfter(act))

  // called when layer is activated/deactivated by the container
  def onActivate(): Unit = activeAt = activeAt.orElse(Some(Instant.now()))

  def onDeactivate(): Unit = activeAt = None

  override def toggleEvent = if (isOn) deactivateEvent else activateEvent

  protected def maybeLightB(b: HasButtonState): Seq[SupBooleanB] =
    val ml = JamControl.maybeLight(b)
    Util.println(s"$id light: $ml $silent")
    ml.filter(_ => !silent).toSeq.map((l: OnOffHardwareLight) => SupBooleanB(l.isOn, () => isOn))

  // let's just say we're too lazy to import cats and make a proper Show instance
  override def toString(): String = s"ML:$id"
}

/** Layer whose (de)activation is controlled by actions
  */
trait ActivatedLayer[+A] {
  def activateEvent: A
  def deactivateEvent: A
  def toggleEvent: A
}

/** (De)activation is triggered by internal actions: must invoke them explicitly
  */
trait IntActivatedLayer extends ActivatedLayer[Seq[ModeCommand[_]]] {
  /* main act/deact events to bind to */
  protected[mode] val selfActivateEvent   = WithSource(ModeCommand.Activate(this), this)
  protected[mode] val selfDeactivateEvent = WithSource(ModeCommand.Deactivate(this), this)

  /* */
  def activateEvent: Vector[ModeCommand[_]]   = selfActivateEvent.value +: maybeActivate
  def deactivateEvent: Vector[ModeCommand[_]] = maybeDeactivate :+ selfDeactivateEvent.value

  private inline def maybeActivate: Vector[ModeCommand[?]] = this match
    case l: HasSubModes => l.subModesToActivate.flatMap(_.activateEvent)
    case _              => Vector.empty

  private inline def maybeDeactivate: Vector[ModeCommand[?]] = this match
    case l: HasSubModes => l.subModesToDeactivate.flatMap(_.deactivateEvent)
    case _              => Vector.empty
}

trait ListeningLayer {
  // all bindings when layer is ready and listening
  val loadBindings: Seq[Binding[_, _, _]]
}

// does not self-activate
abstract class SimpleModeLayer(
  val id: String
)(using MonsterJamExt)
    extends ModeLayer {}

object SimpleModeLayer {
  inline def apply(name: String, modeBindings: Seq[Binding[_, _, _]])(using
    MonsterJamExt
  ): SimpleModeLayer = {
    val x = modeBindings
    new SimpleModeLayer(name) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
    }
  }
}

enum GateMode:
  case Gate, // active only when mode button is pressed
    Toggle,
    Auto,       // toggle on momentary press, gate when held
    OneWay,     // pressing turns mode on
    AutoInverse // toggle on momentary press, ignore release after long press

trait ModeButtonLayer(
  val id: String,
  val modeButton: HasButtonState,
  val gateMode: GateMode = GateMode.Auto,
  override val silent: Boolean = false
)(using ext: MonsterJamExt)
    extends ModeLayer,
      ListeningLayer {
  import GateMode.*
  private var pressedAt: Instant = null
  private object NR:
    // workaround for java.lang.AbstractMethodError: Receiver class com.github.unthingable.jam.Jam$$anon$4 does not define or inherit an implementation of the resolved method 'abstract void noRelease_$eq(boolean)' of interface com.github.unthingable.framework.mode.ModeButtonLayer.
    var noRelease: Boolean = true
  import NR.*

  override val loadBindings: Seq[Binding[_, _, _]] = Vector(
    EB(
      modeButton.st.press,
      s"$id: mode button pressed",
      () => {
        Util.println(" isOn: " + isOn)
        pressedAt = Instant.now()
        gateMode match
          case Auto | Gate =>
            noRelease = false
            if (!isOn) activateEvent else Vector.empty
          case AutoInverse =>
            noRelease = !isOn
            if (!isOn) activateEvent else Vector.empty
          case Toggle =>
            noRelease = true
            if (!isOn) activateEvent else deactivateEvent
          case OneWay =>
            noRelease = true
            if (!isOn) activateEvent else Vector.empty
      }, // bb = BB(exclusive = false)
    ),
    EB(modeButton.st.release, s"$id: mode button released", () => released)
  ) ++ maybeLightB(modeButton)

  private def released: Seq[ModeCommand[_]] =
    inline def isHeldLongEnough = Instant.now().isAfter(pressedAt.plus(Duration.ofMillis(500)))
    inline def operated         = modeBindings.hasOperatedAfter(pressedAt)
    inline def isAlreadyOn      = activeAt.map(pressedAt.isAfter).getOrElse(false)

    if (!isOn || noRelease) Vector.empty
    else
      gateMode match {
        case Gate => deactivateEvent
        case Auto =>
          if (isHeldLongEnough || isAlreadyOn || operated)
            deactivateEvent
          else
            Vector.empty
        case AutoInverse =>
          if (isHeldLongEnough || operated) Vector.empty else deactivateEvent
        case _ => Vector.empty
      }
}

object ModeButtonLayer {
  inline def apply(
    name: String,
    modeButton: JamOnOffButton,
    modeBindings: Seq[Binding[_, _, _]],
    gateMode: GateMode = GateMode.Auto,
    silent: Boolean = false
  )(using MonsterJamExt): ModeButtonLayer = {
    val x = modeBindings
    new ModeButtonLayer(name, modeButton, gateMode, silent) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
      val subActivate: Vector[ModeCommand[_]]          = Vector.empty
      val subDeactivate: Vector[ModeCommand[_]]        = Vector.empty
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
    extends MultiModeLayer {
  protected var isStuck: Boolean = false

  var selected: Option[Int] = Some(0)

  override def subModesToActivate: Vector[ModeLayer] =
    val ret = (selected.map(subModes(_)).toVector ++
      super.subModesToActivate // in case they were bumped
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

  def select(idx: Int): Unit = {
    if (isOn && !selected.contains(idx))
      ext.events.eval(s"$id select deact")(
        selected
          // .filter(_ != idx)
          .map(subModes(_))
          .filter(_.isOn)
          .toSeq
          .flatMap(_.deactivateEvent)*
      )
    val mode = subModes(idx)
    Util.println("sub: " + (if (isOn) "activating" else "selecting") + s" submode ${mode.id}")
    selected = Some(idx)
    if (isOn && !mode.isOn) ext.events.eval(s"$id select act")(mode.activateEvent*)
  }
}

abstract class ModeButtonCycleLayer(
  override val id: String,
  override val modeButton: HasButtonState,
  val cycleMode: CycleMode,
  override val gateMode: GateMode = GateMode.OneWay,
  override val silent: Boolean = false,
  val siblingOperatedModes: Seq[ModeLayer] = Vector(),
)(using ext: MonsterJamExt)
    extends ModeCycleLayer(id),
      ModeButtonLayer(id, modeButton) {

  def stickyPress: Vector[ModeCommand[_]] =
    (isOn, cycleMode: CycleMode) match {
      case (false, _) => activateEvent
      case _          => Vector.empty
    }

  // bindings to inspect when unsticking
  def operatedBindings: Iterable[Binding[_, _, _]] =
    (selected.map(subModes) ++ siblingOperatedModes).flatMap(_.modeBindings)

  def stickyRelease: Vector[ModeCommand[_]] =
    (isOn, cycleMode: CycleMode) match {
      case (true, CycleMode.Gate) => deactivateEvent
      case (true, CycleMode.Sticky) =>
        lazy val operated = hasDirtyBindings(operatedBindings.toSeq*)

        if (isStuck || !isOlderThan(Duration.ofMillis(500)) || operated) {
          isStuck = false
          deactivateEvent
        } else
          isStuck = true
          Vector.empty
      case _ => Vector.empty
    }

  // overrideable
  def lightOn: BooleanSupplier = () => isOn

  // if overriding, remember to include these
  def modeBindings: Seq[Binding[_, _, _]] = cycleMode match {
    case CycleMode.Cycle =>
      Vector(
        EB(modeButton.st.press, s"$id cycle", () => cycle(), BB(tracked = false, exclusive = false))
      )
    case CycleMode.Gate | CycleMode.Sticky =>
      Vector(
        EB(
          modeButton.st.press,
          s"$id gate on",
          () => stickyPress,
          BB(tracked = false, exclusive = false)
        ),
        EB(
          modeButton.st.release,
          s"$id gate off",
          () => stickyRelease,
          BB(tracked = false, exclusive = false)
        )
      )
    case _ => Vector.empty
  }
}
