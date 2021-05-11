package com.github.unthingable.jam

import com.bitwig.extension.controller.api.{HardwareActionBindable, HardwareBindable, HardwareBinding, HardwareBindingSource}

import scala.collection.mutable

case class Binding[S, T](source: S, target: T)

/**
 * A group of control bindings to specific host/app functions that plays well with other layers
 * @param bindings
 */
case class ModeLayer(
  name: String,
  bindings: Map[HardwareBindingSource[_], HardwareBindable] = Map.empty
)

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
