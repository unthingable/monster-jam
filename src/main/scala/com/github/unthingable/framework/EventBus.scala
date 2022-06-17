package com.github.unthingable.framework

import com.github.unthingable.Util
import com.github.unthingable.jam.surface.WithSource

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Stupid simple pubsub
 */

class EventBus[E] {
  type Reactor = E => Unit

  private val subs = mutable.HashMap.empty[E, mutable.HashSet[Reactor]]

  def eval(e: E): Unit =
    val receivers = subs.get(e).toSeq.flatten
    if (receivers.nonEmpty) Util.println(s"evt: $e => ${receivers.size}")
    receivers.foreach(_(e))

  def eval(e: WithSource[E, _]): Unit = eval(e.value)

  def addSub(e: E, r: Reactor): Unit =
    Util.println(s"evt+ $e")
    getSub(e).addOne(r)

  def rmSub(e: E, r: Reactor): Unit = 
    Util.println(s"evt- $e")
    getSub(e).filterInPlace(_ != r)

  def setSub(e: E, r: Reactor): Unit =
    Util.println(s"evt= $e")
    subs.update(e, mutable.HashSet(r))
    
  def clearSub(e: E): Unit =
    Util.println(s"evt clear: $e")
    subs.remove(e)

  private def getSub(e: E): mutable.HashSet[Reactor] = subs.getOrElseUpdate(e, mutable.HashSet.empty[Reactor])
}
