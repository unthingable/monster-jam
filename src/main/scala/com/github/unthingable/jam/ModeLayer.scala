package com.github.unthingable.jam

import com.bitwig.extension.controller.api.{HardwareAction, HardwareActionBindable, HardwareBindable, HardwareBinding, HardwareBindingSource}
import com.github.unthingable.MonsterJamExt

import scala.collection.mutable

trait Modifier
case class ModLayer(modifier: Modifier, layer: ModeLayer)

trait HasModLayers { def modLayers: Seq[ModLayer] }

sealed trait Binding
trait Unbindable // some things just cannot be unbound?
// Controller <- Bitwig
case class InBinding[C, B](source: B, target: C) extends Binding
// Controller -> Bitwig
case class OutBinding[C, B](source: C, target: B) extends Binding

case class InBindings(
  activate: Seq[HardwareAction] = Seq.empty,
  deactivate: Seq[HardwareAction] = Seq.empty,
  load: Seq[_] = Seq.empty,
  unload: Seq[_] = Seq.empty,
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
 * @param outBindings
 */
class ModeLayer(
  val name: String,
  val outBindings: Map[HardwareBindingSource[_ <: HardwareBinding], HardwareBindable] = Map.empty,
  val inBindings: InBindings = InBindings() //Seq[InBinding[_,_]] = Seq.empty,
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

  val bindings: mutable.ArrayDeque[HardwareBinding] = mutable.ArrayDeque.empty
}

trait ModeLayerDSL {
  def bind[S, T](pair: (S, T)*) = ???
}

trait LayerContainer {
  def select(layer: ModeLayer): Unit
  def pop(): Unit
}

trait Stack extends LayerContainer
trait Carousel extends LayerContainer

class LayerStack(base: ModeLayer)(implicit ext: MonsterJamExt) {
  val layers: mutable.ArrayDeque[ModeLayer] = mutable.ArrayDeque(base)
  //val layerMap: mutable.Map[String, ModeLayer] = mutable.Map.empty
  activate(base)

  def load(layer: ModeLayer): Unit = {
    if (layers.contains(layer)) throw new Exception(s"Layer ${layer.name} already loaded")
    layer.inBindings.activate foreach(_.addBinding(activateAction(layer)))
    layer.inBindings.deactivate foreach(_.addBinding(deactivateAction(layer)))
    layers.append(layer)
  }

  def activateAction(layer: ModeLayer): HardwareActionBindable = ext.binding(() => {
    layer.outBindings foreach { case (source, target) =>
      layer.bindings.addOne(source.setBinding(target))
    }
  },
    s"${layer.name} activate")

  def deactivateAction(layer: ModeLayer): HardwareActionBindable = ext.binding(() => {
    layer.bindings.foreach(_.removeBinding())
    // restore base
    activate(base)
  },
    s"${layer.name} deactivate")

  def activate(layer: ModeLayer): Unit = {
    layer.outBindings foreach { case (source, target) =>
      source.setBinding(target)
    }
    //layers.append(layer)
  }
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
