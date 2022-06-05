package com.github.unthingable

package object framework {
  trait HasId:
    def id: String
    // IDs shouldn't repeat among the same set of objects
    override def hashCode(): Int = id.hashCode()
}
