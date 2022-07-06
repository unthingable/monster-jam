package com.github.unthingable.framework.binding

import scala.collection.mutable
import com.github.unthingable.framework.mode.Graph.ModeNode

class Binder {
  // All source elements currently bound by us
  val sourceMap: mutable.HashMap[Any, mutable.HashMap[Binding[_, _, _], ModeNode]] = mutable.HashMap.empty

  def bind(binding: Binding[_,_,_], node: ModeNode): Unit = {
    binding.bind()
    sourceMap.getOrElseUpdate(binding.source, mutable.HashMap.empty).update(binding, node)
  }

  def unbind(binding: Binding[_,_,_]): Unit = {
    binding.clear()
    sourceMap.get(binding.source).foreach(_.remove(binding))
  }
}
