package com.github.unthingable.framework.binding

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.*
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.HasId
import com.github.unthingable.framework.binding.HB.HBS
import com.github.unthingable.framework.mode.Graph
import com.github.unthingable.jam.surface.HasButtonState
import com.github.unthingable.jam.surface.HasHwButton
import com.github.unthingable.jam.surface.HasOnOffLight
import com.github.unthingable.jam.surface.HasRgbLight
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamOnOffButton
import com.github.unthingable.jam.surface.JamRgbButton
import com.github.unthingable.jam.surface.WithSource

import java.time.Instant
import java.util.function.BooleanSupplier
import java.util.function.Supplier
import scala.collection.mutable

import reflect.Selectable.reflectiveSelectable

trait Clearable:
  // stop the binding from doing its thing
  def clear(): Unit

trait Bindable extends Clearable:
  def bind(): Unit // watch out for idempotence

trait Named:
  def name: String

/** Unmanaged/exclusive bindings are to be left alone when modes are deactivated
  *
  * @param managed
  *   bind and stay bound if unmanaged
  * @param exclusive
  *   bumps other bindings
  */
case class BindingBehavior(
  tracked: Boolean = true,
  managed: Boolean = true,
  exclusive: Boolean = true
)

sealed trait Binding[S, B, T] extends Bindable:
  def source: S // exclusivity object, will be indexed by ModeGraph for bumping calculus
  def target: T

  def bindingSource: B // actual thing we're binding to, provided by S

  val behavior: BindingBehavior = BindingBehavior(managed = true, exclusive = true)

// Controller <- Bitwig host
sealed trait InBinding[H, T] extends Binding[H, T, T]:
  def bindingSource: T = target

// Controller -> Bitwig host
sealed trait OutBinding[S, B, H] extends Binding[S, B, H], BindingDSL, Named:
  given ext: MonsterJamExt

  // if a control was operated, it's useful to know for momentary modes
  var operatedAt: Option[Instant] = None

// Bind hardware elements to actions
class HB[S](
  val source: S,
  val toSource: S => HBS,
  val name: String,
  val target: HardwareBindable,
  override val behavior: BindingBehavior
)(using val ext: MonsterJamExt)
    extends OutBinding[S, HBS, HardwareBindable],
      Named:

  override val bindingSource = toSource(source)

  private val bindings: mutable.ArrayDeque[HardwareBinding] = mutable.ArrayDeque.empty
  var isActive                                              = false

  override def bind(): Unit =
    // assert(!isActive)
    if !isActive then
      bindings.addAll(
        Vector(
          bindingSource.addBinding(target)
        ) ++ (if behavior.tracked then
                operatedActions
                  .find(bindingSource.canBindTo)
                  .map(bindingSource.addBinding)
              else Seq.empty)
      )
    isActive = true

  override def clear(): Unit =
    bindings.foreach(_.removeBinding())
    bindings.clear()
    source match
      case x: Clearable => x.clear()
      case _            => ()
    operatedAt = None
    isActive = false

  private val operatedActions = Vector(
    action(() => s"HB $name: unit operated", () => operatedAt = Some(Instant.now())),
    action(() => s"HB $name: double operated", _ => operatedAt = Some(Instant.now())),
    ext.host.createRelativeHardwareControlAdjustmentTarget(_ => operatedAt = Some(Instant.now()))
  )
end HB

object HB extends BindingDSL:
  /*
  HB describes a binding source as a provider and a getter. The provider will get hashed by
  ModGraph for binding bumping.
  Sometimes all you have is a HardwareAction (hopefully a singleton object), then the getter is identity.
  When creating new non-sigleton HAs, make sure they hash properly.
   */
  inline def apply(source: HBS, name: String, target: () => Unit, behavior: BindingBehavior)(implicit
    ext: MonsterJamExt
  ): HB[HBS] =
    new HB(source, identity[HBS], name, action(name, target), behavior)

  inline def apply(source: HBS, name: String, target: () => Unit)(implicit
    ext: MonsterJamExt
  ): HB[HBS] =
    new HB(source, identity[HBS], name, action(name, target), BindingBehavior())

  inline def apply[S](source: S, toSource: S => HBS, name: String, target: () => Unit)(implicit
    ext: MonsterJamExt
  ): HB[S] =
    new HB(source, toSource, name, action(name, target), BindingBehavior())

  inline def apply(source: HBS, name: String, target: HardwareBindable, behavior: BindingBehavior)(implicit
    ext: MonsterJamExt
  ): HB[HBS] =
    new HB(source, identity[HBS], name, target, behavior)

  inline def apply(source: HBS, name: String, target: HardwareBindable)(implicit
    ext: MonsterJamExt
  ): HB[HBS] =
    new HB(source, identity[HBS], name, target, BindingBehavior())
end HB

case class SupColorB(target: MultiStateHardwareLight, source: Supplier[Color])
    extends InBinding[Supplier[Color], MultiStateHardwareLight]:
  override def bind(): Unit = target.setColorSupplier(source)

  override def clear(): Unit = target.setColorSupplier(() => Color.nullColor())

case class SupColorStateB[A <: InternalHardwareLightState](
  target: MultiStateHardwareLight,
  source: Supplier[A],
  empty: A = JamColorState.empty
) extends InBinding[Supplier[A], MultiStateHardwareLight]:
  override def bind(): Unit = target.state.setValueSupplier(source)

  override def clear(): Unit = target.state.setValueSupplier(() => empty)

case class SupBooleanB(target: BooleanHardwareProperty, source: BooleanSupplier)
    extends InBinding[BooleanSupplier, BooleanHardwareProperty]:
  override def bind(): Unit = target.setValueSupplier(source)

  override def clear(): Unit = target.setValueSupplier(() => false)

object JCB extends BindingDSL:
  /*
  Helper binding combinations, when you don't need to inspect Binder state
   */
  inline def apply(
    name: String,
    b: HasButtonState & HasRgbLight,
    press: () => Unit,
    release: () => Unit,
    color: Supplier[JamColorState]
  )(using MonsterJamExt) =
    Vector(
      SupColorStateB(b.light, color),
      EB(b.st.press, s"$name press", press),
      EB(b.st.release, s"$name release", release),
    )

  inline def apply(
    name: String,
    b: HasButtonState & HasOnOffLight,
    press: () => Unit,
    release: () => Unit,
    isOn: BooleanSupplier
  )(using MonsterJamExt) =
    Vector(
      SupBooleanB(b.light.isOn, isOn),
      EB(b.st.press, s"$name press", press),
      EB(b.st.release, s"$name release", release),
    )

  inline def empty(b: HasButtonState & HasOnOffLight & HasId, isOn: BooleanSupplier = () => false)(using
    MonsterJamExt
  ) =
    Vector(
      SupBooleanB(b.light.isOn, isOn),
      EB(b.st.press, s"${b.id} press noop", () => Seq.empty),
      EB(b.st.release, s"${b.id} release noop", () => Seq.empty),
    )
end JCB

// event binding
case class EB[S](
  source: S,
  ev: Event,
  action: Outcome,
  override val behavior: BindingBehavior,
  val name: String
)(using val ext: MonsterJamExt)
    extends OutBinding[S, Event, Outcome]:
  var isActive = false

  val receiver = (_: Event) =>
    if name.nonEmpty then Util.println(s"EB: $name $action")
    val msg = s"$name->$action"
    operatedAt = Some(Instant.now())
    action match
      case x: Command    => ext.events.eval(msg)(x)
      case SideEffect(f) => f(ev)
      case CmdEffect(f)  => ext.events.eval(msg)(f(ev)*)

  override def bind(): Unit =
    if !isActive then ext.events.addSub(ev, receiver)
    isActive = true

  override val target: Outcome = action

  override val bindingSource: Event = ev

  override def clear(): Unit =
    given Util.SelfEqual[Event => Unit] = CanEqual.derived
    ext.events.rmSub(ev, receiver)
    operatedAt = None
    isActive = false
end EB

object EB:
  type EventSpec[S] = Event | (S, S => Event)

  type EventSpecOut[A, S] = A match
    case Event           => Event
    case (S, S => Event) => S

  type OutcomeSpec = Command | (() => Unit) | (() => Seq[Command])

  inline def asOutcome(o: OutcomeSpec): Outcome =
    inline o match
      case c: Command              => c
      case f: (() => Seq[Command]) => CmdEffect(_ => f())
      case f: (() => Unit)         => SideEffect(_ => f())

  inline def apply[S](ev: WithSource[Event, S], ctx: String, f: OutcomeSpec)(using
    MonsterJamExt
  ): EB[S] =
    EB(ev.source, ev.value, asOutcome(f), behavior = BindingBehavior(), name = ctx)
  inline def apply[S](ev: WithSource[Event, S], ctx: String, f: OutcomeSpec, bb: BindingBehavior)(using
    MonsterJamExt
  ): EB[S] =
    EB(ev.source, ev.value, asOutcome(f), behavior = bb, name = ctx)
end EB
