package com.github.unthingable.framework

import scala.collection.mutable
import com.github.unthingable.Util
import scala.collection.mutable.ListBuffer

/**
 * Stupid simple pubsub
 */

class EventBus[E] {
  type Reactor = E => Unit

  private val subs = mutable.HashMap.empty[E, mutable.ListBuffer[Reactor]]

  def pub(e: E): Unit =
    Util.println("evt: " + e.toString)
    subs.get(e).toSeq.flatten.foreach(_(e))

  def addSub(e: E, r: Reactor): Unit =
    getSub(e).addOne(r)

  def rmSub(e: E, r: Reactor): Unit = getSub(e).filterInPlace(_ != r)

  def setSub(e: E, r: Reactor): Unit =
    subs.update(e, mutable.ListBuffer(r))
    
  def clearSub(e: E): Unit =
    subs.remove(e)

  private def getSub(e: E): mutable.ListBuffer[Reactor] = subs.getOrElseUpdate(e, mutable.ListBuffer.empty[Reactor])
}
