package com.github.unthingable.framework.binding

import com.github.unthingable.framework.mode.Graph.ModeNode

import scala.collection.mutable

class Binder:
  // All source elements currently bound by us
  val sourceMap: mutable.HashMap[Any, mutable.HashMap[Binding[?, ?, ?], ModeNode]] = mutable.HashMap.empty

  def bind(binding: Binding[?, ?, ?], node: ModeNode): Unit =
    binding.bind()
    sourceMap.getOrElseUpdate(binding.source, mutable.HashMap.empty).update(binding, node)

  def unbind(binding: Binding[?, ?, ?]): Unit =
    binding.clear()
    sourceMap.get(binding.source).foreach(_.remove(binding))
