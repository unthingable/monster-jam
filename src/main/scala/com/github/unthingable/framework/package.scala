package com.github.unthingable

package object framework:

  opaque type ButtonId = String

  object ButtonId:
    def apply(s: String): ButtonId = s
  trait HasId:
    def id: String

    // IDs shouldn't repeat among the same set of objects
    final override def hashCode(): Int = id.hashCode()
