package com.github.unthingable.jam.binding

import scala.collection.mutable

class Binder {
  // All source elements currently bound by us
  val sourceMap: mutable.HashMap[Any, mutable.HashSet[Binding[_, _, _]]] = mutable.HashMap.empty

  def bind(binding: Binding[_,_,_]): Boolean = {
    binding.bind()
    sourceMap.getOrElseUpdate(binding.source, mutable.HashSet.empty).add(binding)
  }

  def unbind(binding: Binding[_,_,_]): Unit = {
    binding.clear()
    sourceMap.get(binding.source).foreach(_.remove(binding))
  }
}
