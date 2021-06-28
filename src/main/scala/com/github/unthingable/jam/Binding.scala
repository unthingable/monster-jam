package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.{BooleanValueChangedCallback, ColorValueChangedCallback, ValueChangedCallback}
import com.bitwig.extension.controller.api.{BooleanHardwareProperty, HardwareActionBindable, HardwareBindable, HardwareBinding, HardwareBindingSource, InternalHardwareLightState, MultiStateHardwareLight, Value}
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.BindingDSL.HBS

import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.mutable
import com.github.unthingable.jam.surface.{FakeAction, JamColorState}

trait Clearable {
  // stop the binding from doing its thing
  def clear(): Unit
}

trait Named {
  def name: String
}

/**
 * Unmanaged/exclusive bindings are to be left alone when modes are removed
 *
 * @param managed bind and stay bound
 * @param exclusive bumps other bindings
 */
case class BindingBehavior(
  managed: Boolean = true,
  exclusive: Boolean = true
)

sealed trait Binding[S, T, I] extends Clearable {
  def bind(): Unit // watch out for idempotence
  def source: S
  def target: T

  def surfaceElem: I

  var node: Option[Graph.ModeNode] = None // backreference to the node that owns this
  def layerName: String = node.map(_.layer.name).getOrElse("")

  val behavior: BindingBehavior = BindingBehavior(managed = true, exclusive = true)
}

// Controller <- Bitwig host
sealed trait InBinding[H, C] extends Binding[H, C, C] {
  def surfaceElem: C = target
}

// Controller -> Bitwig host
sealed trait OutBinding[C, H] extends Binding[C, H, C] with BindingDSL {
  def surfaceElem: C = source // might be weird with observer bindings
  implicit val ext: MonsterJamExt

  // if a control was operated, it's useful to know for momentary modes
  var wasOperated: Boolean = false
}

// Bind hardware elements to actions
case class HB(
  source: HBS,
  name: String, target: HardwareBindable,
  tracked: Boolean = true,
  override val behavior: BindingBehavior = BindingBehavior())
  (implicit val ext: MonsterJamExt)
  extends OutBinding[HBS, HardwareBindable] with Named {

  private val bindings: mutable.ArrayDeque[HardwareBinding] = mutable.ArrayDeque.empty
  var isActive = false

  override def bind(): Unit = {
    //assert(!isActive)
    if (!isActive)
      bindings.addAll(
        Vector(
          source.addBinding(target)
        ) ++ (if (tracked)
                operatedActions
                  .find(source.canBindTo)
                  .map(source.addBinding)
              else Seq.empty)
      )
    isActive = true
  }

  override def clear(): Unit = {
    bindings.foreach(_.removeBinding())
    bindings.clear()
    //source.clearBindings() // one of these is probably unnecessary
    wasOperated = false
    isActive = false
  }

  private val operatedActions = Vector(
    action(() => s"$layerName: HB: unit operated", () => {wasOperated = true}),
    action(() => s"$layerName: HB: double operated", _ => {wasOperated = true}),
    ext.host.createRelativeHardwareControlAdjustmentTarget(_ => {wasOperated = true})
  )
}

object HB extends BindingDSL {
  def apply(source: HBS, name: String, target: () => Unit)
    (implicit ext: MonsterJamExt): HB =
    new HB(source, name, action(name, target))
  def apply(source: HBS, name: String, target: () => Unit, tracked: Boolean)
    (implicit ext: MonsterJamExt): HB =
    new HB(source, name, action(name, target), tracked = tracked)
  def apply(source: HBS, name: String, target: () => Unit, tracked: Boolean, behavior: BindingBehavior)
    (implicit ext: MonsterJamExt): HB =
    new HB(source, name, action(name, target), tracked = tracked, behavior = behavior)
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

//
//class ObserverB[S, T, C](val source: S, val target: T, binder: (S, C) => Unit, receiver: C, empty: C)
//  extends InBinding[S, T] {
//  // observers are not removable, so
//  private var action: C = empty
//  binder(source, action)
//
//  override def bind(): Unit = action = receiver
//
//  override def clear(): Unit = action = empty
//}
//
//// not working?
//case class ValObserverB[A <: ValueChangedCallback, T](value: Value[A], receiver: A, override val target: T)
//  (implicit emp: EmptyCB[A])
//  extends ObserverB[Value[A], T, A](value, target, _.addValueObserver(_), receiver, emp.empty)
//
//case class LoadActions(
//  activate: HBS,
//  deactivate: HBS,
//)

trait BindingDSL {
  def action(name: String, f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => {
      ext.host.println(s"! $name")
      f()
    }, () => name)

  def action(name: Supplier[String], f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => {
      ext.host.println(s"! ${name.get()}")
      f()
    }, name)

  def action(name: String, f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), () => name)

  def action(name: Supplier[String], f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), name)

  //implicit class PolyAction(a: HardwareAction)(implicit ext: MonsterJamExt) {
  //  def addBinding(f: () => Unit): HardwareActionBinding = a.addBinding(action("", () => f()))
  //}

  type HBS = HardwareBindingSource[_ <: HardwareBinding]

  trait EmptyCB[A <: ValueChangedCallback] { def empty: A }

  implicit object emptyBool extends EmptyCB[BooleanValueChangedCallback] {
    override def empty: BooleanValueChangedCallback = _ => ()
  }

  implicit object emptyColor extends EmptyCB[ColorValueChangedCallback] {
    override def empty: ColorValueChangedCallback = (_,_,_) => ()
  }

  // fake action detector (optimize later)
  def isFakeAction(source: Any): Boolean = source match {
    case a: FakeAction => true // !a.masquerade
    case _             => false
  }
}

object BindingDSL extends BindingDSL
