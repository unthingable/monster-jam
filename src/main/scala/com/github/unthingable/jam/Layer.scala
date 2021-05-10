package com.github.unthingable.jam

import com.bitwig.extension.controller.api.{HardwareActionBindable, HardwareBindable, HardwareBinding, HardwareBindingSource}

import scala.collection.mutable

case class Binding[S, T](source: S, target: T)

case class Layer(
  bindings: Map[HardwareBindingSource[_], HardwareBindable] = Map.empty
)

object LayerStack {
  val layers: mutable.ArrayDeque[Layer] = mutable.ArrayDeque.empty

  def push(layer: Layer): Unit = {
    layer.bindings foreach { case (source, target) =>
      source.setBinding(target)
    }
    layers.append(layer)
  }

  def pop(): Unit = {
    if (layers.nonEmpty) {
      val layer = layers.removeLast()
      layer.bindings.keys.foreach(_.clearBindings())
    }
    // rebind last
    if (layers.nonEmpty) {
      val layer = layers.removeLast()
      push(layer)
    }
  }
}
