package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{HardwareAction, HardwareActionBindable, HardwareBindable, HardwareBinding, HardwareBindingSource, MultiStateHardwareLight}
import com.github.unthingable.MonsterJamExt

import java.util.function.Supplier
import scala.collection.mutable

trait Modifier
case class ModLayer(modifier: Modifier, layer: ModeLayer)

trait HasModLayers { def modLayers: Seq[ModLayer] }

trait Clearable {
  def clear(): Unit
}

sealed trait Binding[C, H] {
  //def isBound: Boolean
  def bind(): Unit
  def source: C
  def target: H
}

trait Unbindable // some things just cannot be unbound?
// Controller <- Bitwig host
sealed trait InBinding[H, C] extends Binding[C, H]
// Controller -> Bitwig host
sealed trait OutBinding[C, H] extends Binding[C, H]

case class HWB(source: HardwareBindingSource[_ <: HardwareBinding], target: HardwareBindable)
  extends OutBinding[HardwareBindingSource[_ <: HardwareBinding], HardwareBindable] with Clearable {
  private var binding: Option[_ <: HardwareBinding] = None
  override def bind(): Unit = { binding = Some(source.setBinding(target)) }

  override def clear(): Unit = binding.foreach(_.removeBinding())
}

case class SupColor(target: MultiStateHardwareLight, source: Supplier[Color])
  extends InBinding[MultiStateHardwareLight, Supplier[Color]] {
  override def bind(): Unit = target.setColorSupplier(source)
}

case class LoadBindings(
  activate: Seq[HardwareAction] = Seq.empty,
  deactivate: Seq[HardwareAction] = Seq.empty,
  //load: Seq[_] = Seq.empty,
  //unload: Seq[_] = Seq.empty,
)

/*
Layer behavior modification strategies:
1 inherent behavior defined by layer's own bindings
2 poll: layer proactively observes modifiers value
3 push: modifier notifications are pushed to layer (redundant because can be done with 1)
4 subscribe: layer observers other modifiers value, possibly overriding modifier's behavior? (example: SOLO mode pin by SONG)

Layers and modifiers can interact in complex ways.

Layer combinator types:
- carousel: only one layer (of several) can be active at a time
- stackable: active layers can be added to and removed from the top of the stack, overriding bindings

A layer container controls how its layers combine

Panel: a group of layers for a specific area of Jam
 */

/**
 * A group of control bindings to specific host/app functions that plays well with other layers
 * @param modeBindings
 */
class ModeLayer(
  val name: String,
  val modeBindings: Seq[Binding[_,_]] = Seq.empty,
  val loadBindings: LoadBindings = LoadBindings() //Seq[InBinding[_,_]] = Seq.empty,
)(implicit ext: MonsterJamExt) {
  // activate in bindings
  def load(): Unit = ???
  def unload(): Unit = ???
  // activate out bindings
  def activate(): Unit = ???
  def deactivate(): Unit = ???

  val loadAction: HardwareActionBindable = ext.binding(() => load(), s"$name load")
  val unloadAction: HardwareActionBindable = ext.binding(() => unload(), s"$name unload")
  val activateAction: HardwareActionBindable = ext.binding(() => activate(), s"$name activate")
  val deactivateAction: HardwareActionBindable = ext.binding(() => deactivate(), s"$name deactivate")

  val bindings: mutable.ArrayDeque[Binding[_,_]] = mutable.ArrayDeque.empty
}

trait ModeLayerDSL {
  def bind[S, T](pair: (S, T)*) = ???
  def action(name: String, f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable = ext.binding(f, name)
}

trait LayerContainer {
  def select(layer: ModeLayer): Unit
  def pop(): Unit
}

trait Stack extends LayerContainer
trait Carousel extends LayerContainer

class LayerStack(base: ModeLayer*)(implicit ext: MonsterJamExt) {
  val layers: mutable.ArrayDeque[ModeLayer] = mutable.ArrayDeque.empty
  //val layerMap: mutable.Map[String, ModeLayer] = mutable.Map.empty
  activateBase()

  def load(layer: ModeLayer): Unit = {
    if (layers.contains(layer)) throw new Exception(s"Layer ${layer.name} already loaded")
    layer.loadBindings.activate foreach(_.addBinding(activateAction(layer)))
    layer.loadBindings.deactivate foreach(_.addBinding(deactivateAction(layer)))
    layers.append(layer)
  }

  def activateAction(layer: ModeLayer): HardwareActionBindable = ext.binding(() => {
    activate(layer)
  },
    s"${layer.name} activate")

  def deactivateAction(layer: ModeLayer): HardwareActionBindable = ext.binding(() => {
    layer.bindings.foreach { case b: Clearable => b.clear() }
    // restore base
    activateBase()
  },
    s"${layer.name} deactivate")

  def activate(layer: ModeLayer): Unit = {
    layer.modeBindings.foreach(_.bind())
  }

  def activateBase(): Unit =
    base.flatMap(_.modeBindings).foreach(_.bind())
  //
  //def pop(layer: ModeLayer): Unit = {
  //  if (layers.nonEmpty) {
  //    if (layers.last != layer) throw new Exception("Cannot pop not self")
  //    layers.removeLast()
  //    layer.outBindings.keys.foreach(_.clearBindings())
  //  }
  //  // rebind last
  //  if (layers.nonEmpty) {
  //    val layer = layers.removeLast()
  //    push(layer)
  //  }
  //}
}
