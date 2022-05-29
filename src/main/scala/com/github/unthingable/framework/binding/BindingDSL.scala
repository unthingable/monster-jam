package com.github.unthingable.framework.binding

import com.bitwig.extension.callback.{BooleanValueChangedCallback, ColorValueChangedCallback, ValueChangedCallback}
import com.bitwig.extension.controller.api.{HardwareActionBindable, HardwareBinding, HardwareBindingSource}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface.FakeAction

import java.time.Instant
import java.util.function.Supplier

trait ActionDSL {
  def action(name: String, f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => {
      if name.nonEmpty then Util.println(s"! $name")
      f()
    }, () => name)

  def action(name: Supplier[String], f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => {
      Util.println(s"! ${name.get()}")
      f()
    }, name)

  def action(name: String, f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), () => name)

  def action(name: Supplier[String], f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), name)
}
trait BindingDSL extends ActionDSL {
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

  implicit class BindingOps(bindings: Iterable[Binding[_,_,_]]) {
    def inBindings: Iterable[InBinding[_,_]]   = bindings.collect { case x: InBinding[_,_] => x}
    def outBindings: Iterable[OutBinding[_,_,_]] = bindings.collect { case x: OutBinding[_,_,_] => x}
    def operatedAfter(instant: Instant): Boolean =
      bindings.outBindings.exists(_.operatedAt.exists(_.isAfter(instant)))
  }
}

object ActionDSL extends ActionDSL
object BindingDSL extends BindingDSL
