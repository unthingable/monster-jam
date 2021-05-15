package com.github.unthingable.jam

import com.bitwig.extension.controller.api.{HardwareActionBindable, HardwareBindable, HardwareBinding, HardwareBindingSource}

import scala.collection.mutable

trait Modifier
case class ModLayer(modifier: Modifier, layer: ModeLayer)

trait HasModLayers { def modLayers: Seq[ModLayer] }

sealed trait Binding
// Controller <- Bitwig
case class InBinding[C, B](source: B, target: C) extends Binding
// Controller -> Bitwig
case class OutBinding[C, B](source: C, target: B) extends Binding

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
 * @param bindings
 */
case class ModeLayer(
  name: String,
  bindings: Map[HardwareBindingSource[_], HardwareBindable] = Map.empty
)

trait LayerContainer {
  def select(layer: ModeLayer): Unit
  def pop(): Unit
}

trait Stack extends LayerContainer
trait Carousel extends LayerContainer

object LayerStack {
  val layers: mutable.ArrayDeque[ModeLayer] = mutable.ArrayDeque.empty
  //val layerMap: mutable.Map[String, ModeLayer] = mutable.Map.empty

  def push(layer: ModeLayer): Unit = {
    if (layers.contains(layer)) throw new Exception(s"Layer ${layer.name} already on stack")
    layer.bindings foreach { case (source, target) =>
      source.setBinding(target)
    }
    layers.append(layer)
  }

  def pop(layer: ModeLayer): Unit = {
    if (layers.nonEmpty) {
      if (layers.last != layer) throw new Exception("Cannot pop not self")
      layers.removeLast()
      layer.bindings.keys.foreach(_.clearBindings())
    }
    // rebind last
    if (layers.nonEmpty) {
      val layer = layers.removeLast()
      push(layer)
    }
  }
}
