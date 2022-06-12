package com.github.unthingable.framework.binding

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.*
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.framework.binding.HB.HBS
import com.github.unthingable.framework.mode.Graph
import com.github.unthingable.jam.surface.{HasHwButton, HasOnOffLight, HasRgbLight, JamColorState, JamOnOffButton, JamRgbButton}

import java.time.Instant
import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.mutable
import reflect.Selectable.reflectiveSelectable

trait Clearable {
  // stop the binding from doing its thing
  def clear(): Unit
}

trait Named {
  def name: String
}

/**
 * Unmanaged/exclusive bindings are to be left alone when modes are deactivated
 *
 * @param managed bind and stay bound if unmanaged
 * @param exclusive bumps other bindings
 */
case class BindingBehavior(
  tracked: Boolean = true,
  managed: Boolean = true,
  exclusive: Boolean = true
)

sealed trait Binding[S, B, T] extends Clearable {
  def bind(): Unit // watch out for idempotence
  def source: S // will be indexed by ModeGraph for bumping calculus
  def target: T

  def bindingSource: B // actual thing we're binding to, provided by S

  var node: Option[Graph.ModeNode] = None // backreference to the node that owns this
  def layerName: String = node.map(_.layer.id).getOrElse("")

  val behavior: BindingBehavior = BindingBehavior(managed = true, exclusive = true)
}

// Controller <- Bitwig host
sealed trait InBinding[H, T] extends Binding[H, T, T] {
  def bindingSource: T = target
}

// Controller -> Bitwig host
sealed trait OutBinding[S, B, H] extends Binding[S, B, H], BindingDSL {
  given ext: MonsterJamExt

  // if a control was operated, it's useful to know for momentary modes
  var operatedAt: Option[Instant] = None
}

// Bind hardware elements to actions
class HB[S](
  val source: S,
  val toSource: S => HBS,
  val name: String,
  val target: HardwareBindable,
  override val behavior: BindingBehavior)
  (using val ext: MonsterJamExt)
  extends OutBinding[S, HBS, HardwareBindable], Named {

  override val bindingSource = toSource(source)

  private val bindings: mutable.ArrayDeque[HardwareBinding] = mutable.ArrayDeque.empty
  var isActive = false

  override def bind(): Unit = {
    //assert(!isActive)
    if (!isActive)
      bindings.addAll(
        Vector(
          bindingSource.addBinding(target)
        ) ++ (if (behavior.tracked)
                operatedActions
                  .find(bindingSource.canBindTo)
                  .map(bindingSource.addBinding)
              else Seq.empty)
      )
    isActive = true
  }

  override def clear(): Unit = {
    bindings.foreach(_.removeBinding())
    bindings.clear()
    source match {
      case x: Clearable => x.clear()
      case _ => ()
    }
    operatedAt = None
    isActive = false
  }

  private val operatedActions = Vector(
    action(() => s"$layerName: HB $name: unit operated", () => {operatedAt = Some(Instant.now())}),
    action(() => s"$layerName: HB $name: double operated", _ => {operatedAt = Some(Instant.now())}),
    ext.host.createRelativeHardwareControlAdjustmentTarget(_ => {operatedAt = Some(Instant.now())})
  )
}

object HB extends BindingDSL {
  /*
  HB describes a binding source as a provider and a getter. The provider will get hashed by
  ModGraph for binding bumping.
  Sometimes all you have is a HardwareAction (hopefully a singleton object), then the getter is identity.
  When creating new non-sigleton HAs, make sure they hash properly.
   */
  inline def apply(source: HBS, name: String, target: () => Unit, behavior: BindingBehavior)
    (implicit ext: MonsterJamExt): HB[HBS] =
    new HB(source, identity[HBS], name, action(name, target), behavior)

  inline def apply(source: HBS, name: String, target: () => Unit)
    (implicit ext: MonsterJamExt): HB[HBS] =
    new HB(source, identity[HBS], name, action(name, target), BindingBehavior())

  inline def apply[S](source: S, toSource: S => HBS, name: String, target: () => Unit)
    (implicit ext: MonsterJamExt): HB[S] =
    new HB(source, toSource, name, action(name, target), BindingBehavior())

  inline def apply(source: HBS, name: String, target: HardwareBindable, behavior: BindingBehavior)
    (implicit ext: MonsterJamExt): HB[HBS] =
    new HB(source, identity[HBS], name, target, behavior)

  inline def apply(source: HBS, name: String, target: HardwareBindable)
    (implicit ext: MonsterJamExt): HB[HBS] =
    new HB(source, identity[HBS], name, target, BindingBehavior())

  // FIXME - temporary shim for compilation
  inline def apply(source: Event, name: String, target: () => Unit)(using MonsterJamExt) = EB(source, target())
  inline def apply(source: Event, name: String, target: () => Unit, behavior: BindingBehavior)(using MonsterJamExt) = EB(source, source, SideEffect(_ => target()), behavior)
}

case class SupColorB(target: MultiStateHardwareLight, source: Supplier[Color])
  extends InBinding[Supplier[Color], MultiStateHardwareLight] {
  override def bind(): Unit = target.setColorSupplier(source)

  override def clear(): Unit = target.setColorSupplier(() => Color.nullColor())
}

case class SupColorStateB[A <: InternalHardwareLightState](
  target: MultiStateHardwareLight, source: Supplier[A], empty: A = JamColorState.empty)
  extends InBinding[Supplier[A], MultiStateHardwareLight] {
  override def bind(): Unit = target.state.setValueSupplier(source)

  override def clear(): Unit = target.state.setValueSupplier(() => empty)
}

case class SupBooleanB(target: BooleanHardwareProperty, source: BooleanSupplier)
  extends InBinding[BooleanSupplier, BooleanHardwareProperty] {
  override def bind(): Unit = target.setValueSupplier(source)

  override def clear(): Unit = target.setValueSupplier(() => false)
}

object JCB extends BindingDSL {
  /*
  Helper binding combinations, when you don't need to inspect Binder state
   */
  inline def apply(name: String, b: HasRgbLight & HasHwButton, press: () => Unit, release: () => Unit, color: Supplier[JamColorState])
    (using MonsterJamExt) =
    Vector(
      SupColorStateB(b.light, color),
      HB(b.btn.pressedAction, s"$name press", press),
      HB(b.btn.releasedAction, s"$name release", release),
    )

  inline def apply(name: String, b: HasOnOffLight & HasHwButton, press: () => Unit, release: () => Unit, isOn: BooleanSupplier)
    (using MonsterJamExt) =
    Vector(
      SupBooleanB(b.light.isOn, isOn),
      HB(b.btn.pressedAction, s"$name press", press),
      HB(b.btn.releasedAction, s"$name release", release),
    )
}

// event binding
// import Event.*
case class EB[S](
  source: S,
  ev: Event, 
  action: Outcome,
  override val behavior: BindingBehavior = BindingBehavior()
)(using val ext: MonsterJamExt) extends OutBinding[S, Event, Outcome] {
  var isActive = false

  val receiver = (_: Event) => action match
      case x: SideEffect => x.f(ev)
      case x: Command => ext.events.eval(x)

  override def bind(): Unit = 
    if (!isActive) ext.events.addSub(ev, receiver)
    isActive = true

  // TODO refactor ownership and exclusivity
  /* WIP 
  source tell the binder how to tell when another mode is taking over the control
  most times a new mode will "shadow" the binding, but some bindings are non-exclusive, curently mnanaged by BindingBehavior
  */
  // override val source: S = source

  override val target: Outcome = action

  override val bindingSource: Event = ev

  // override def clear(): Unit = ext.events.clearSub(ev) // FIXME
  override def clear(): Unit = 
    ext.events.rmSub(ev, receiver)
    isActive = false

  //def eval(): Unit = action match
  //  case f: SideEffect => f(ev)
  //  case _ => ???
}

object EB:
  // inline def apply(ev: Event, f: => Unit)(using MonsterJamExt): EB[Event] = EB(ev, ev, SideEffect(_ => f))
  inline def apply(ev: Event, f: => Unit)(using MonsterJamExt): EB[Event] = EB(ev, ev, SideEffect(_ => f))
  inline def apply(ev: Event, f: => Unit, bb: BindingBehavior)(using MonsterJamExt): EB[Event] = EB(ev, ev, SideEffect(_ => f), bb)
  inline def apply[S](source: S, ev: S => Event, f: => Unit)(using MonsterJamExt): EB[S] = EB(source, ev(source), SideEffect(_ => f))
  inline def apply[S](source: S, ev: S => Event, f: => Unit, bb: BindingBehavior)(using MonsterJamExt): EB[S] = EB(source, ev(source), SideEffect(_ => f), bb)
