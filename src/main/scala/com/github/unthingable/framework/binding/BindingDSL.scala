package com.github.unthingable.framework.binding

import com.bitwig.extension.callback.BooleanValueChangedCallback
import com.bitwig.extension.callback.ColorValueChangedCallback
import com.bitwig.extension.callback.ValueChangedCallback
import com.bitwig.extension.controller.api.HardwareActionBindable
import com.bitwig.extension.controller.api.HardwareBinding
import com.bitwig.extension.controller.api.HardwareBindingSource
import com.bitwig.`extension`.controller.api.RelativeHardwarControlBindable
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.jam.surface.FakeAction

import java.time.Instant
import java.util.function.Supplier

transparent trait ActionDSL:
  def action(name: String, f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(
      () =>
        if name.nonEmpty then Util.println(s"! $name")
        f()
      ,
      () => name
    )

  def action(name: Supplier[String], f: () => Unit)(implicit
    ext: MonsterJamExt
  ): HardwareActionBindable =
    ext.host.createAction(
      () =>
        Util.println(s"! ${name.get()}")
        f()
      ,
      name
    )

  def action(name: String, f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), () => name)

  def action(name: Supplier[String], f: Double => Unit)(implicit
    ext: MonsterJamExt
  ): HardwareActionBindable =
    ext.host.createAction(f(_), name)
end ActionDSL
transparent trait BindingDSL extends ActionDSL:
  type HBS = HardwareBindingSource[? <: HardwareBinding]

  trait EmptyCB[A <: ValueChangedCallback]:
    def empty: A

  implicit object emptyBool extends EmptyCB[BooleanValueChangedCallback]:
    override def empty: BooleanValueChangedCallback = _ => ()

  implicit object emptyColor extends EmptyCB[ColorValueChangedCallback]:
    override def empty: ColorValueChangedCallback = (_, _, _) => ()

  // fake action detector (optimize later)
  inline def isFakeAction(source: Any): Boolean = inline source match
    case a: FakeAction => true // !a.masquerade
    case _             => false

  implicit class BindingOps(bindings: Iterable[Binding[?, ?, ?]]):
    def inBindings: Iterable[InBinding[?, ?]] = bindings.collect { case x: InBinding[?, ?] => x }
    def outBindings: Iterable[OutBinding[?, ?, ?]] = bindings.collect {
      case x: OutBinding[?, ?, ?] => x
    }
    def operatedAfter(instant: Instant): Iterable[OutBinding[?, ?, ?]] =
      bindings.outBindings.view.filter(_.operatedAt.exists(_.isAfter(instant)))

    def hasOperatedAfter(instant: Instant): Boolean =
      bindings.outBindings.view.exists(_.operatedAt.exists(_.isAfter(instant)))

  inline def stepTarget(inc: () => Unit, dec: () => Unit)(using
    ext: MonsterJamExt
  ): RelativeHardwarControlBindable =
    ext.host.createRelativeHardwareControlStepTarget(
      action("", inc),
      action("", dec)
    )
end BindingDSL

object ActionDSL  extends ActionDSL
object BindingDSL extends BindingDSL
