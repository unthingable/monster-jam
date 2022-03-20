package com.github.unthingable.jam.binding

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api._
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.Graph
import com.github.unthingable.jam.binding.HB.HBS
import com.github.unthingable.jam.surface.{JamColorState, JamOnOffButton, JamRgbButton, OnOffButton, RgbButton}

import java.time.Instant
import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.mutable

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
  var operatedAt: Option[Instant] = None
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
    operatedAt = None
    isActive = false
  }

  private val operatedActions = Vector(
    action(() => s"$layerName: HB: unit operated", () => {operatedAt = Some(Instant.now())}),
    action(() => s"$layerName: HB: double operated", _ => {operatedAt = Some(Instant.now())}),
    ext.host.createRelativeHardwareControlAdjustmentTarget(_ => {operatedAt = Some(Instant.now())})
  )
}

object HB extends BindingDSL {
  def apply(source: HBS, name: String, target: () => Unit,
    tracked: Boolean = true, managed: Boolean = true, exclusive: Boolean = true)
    (implicit ext: MonsterJamExt): HB =
    new HB(source, name, action(name, target), tracked, BindingBehavior(managed, exclusive))
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

object JB extends BindingDSL {
  def apply(name: String, b: RgbButton, press: () => Unit, release: () => Unit, color: Supplier[JamColorState])
    (implicit ext: MonsterJamExt) =
    Vector(
      SupColorStateB(b.light, color),
      HB(b.btn.pressed, s"$name press", press),
      HB(b.btn.released, s"$name release", release),
    )

  def apply(name: String, b: OnOffButton, press: () => Unit, release: () => Unit, isOn: BooleanSupplier)
    (implicit ext: MonsterJamExt) =
    Vector(
      SupBooleanB(b.light.isOn, isOn),
      HB(b.btn.pressed, s"$name press", press),
      HB(b.btn.released, s"$name release", release),
    )
}
