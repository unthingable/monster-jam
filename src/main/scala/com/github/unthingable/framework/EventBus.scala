package com.github.unthingable.framework

import com.github.unthingable.Util
import com.github.unthingable.jam.surface.WithSource

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.annotation.targetName

class EventBus[E] {
  type Reactor = E => Unit
  case class EvtContext[E](e: E, ctx: String)

  private val subs = mutable.HashMap.empty[E, mutable.HashSet[Reactor]]

  /* Janky actor with a queue, no mind paid to concurrency.
     Assuming the extention is single-threaded this should be fine.
   */
  private val queue = mutable.Queue.empty[EvtContext[E]]
  private var semaphore: Boolean = false

  private def trigger(): Unit =
    if !semaphore then
      semaphore = true
      evalQueue()

  private def evalQueue(): Unit = 
    while (semaphore && queue.nonEmpty)
      val e = queue.dequeue()
      val ev = e.e
      Util.println(s"evt: $e")
      val receivers = subs.get(ev).toSeq.flatten
      if (receivers.nonEmpty) Util.println(s"evt: $ev => ${receivers.size} ${e.ctx}")
      receivers.foreach(_(ev))
    semaphore = false

  def eval(context: String, e: E*): Unit =
    Util.println(s"evt: enqueueing $e with $context")
    queue.enqueueAll(e.map(EvtContext(_, context)))
    trigger()

  def eval(e: E*): Unit = eval("", e*)

  @targetName("evalS")
  def eval(e: WithSource[E, _]*): Unit = eval(e.map(_.value)*)

  @targetName("evalS")
  def eval(context: String, e: WithSource[E, _]*): Unit = eval(context, e.map(_.value)*)

  def addSub(e: E, r: Reactor): Unit =
    getSub(e).addOne(r)

  def rmSub(e: E, r: Reactor): Unit = 
    getSub(e).filterInPlace(_ != r)

  def setSub(e: E, r: Reactor): Unit =
    subs.update(e, mutable.HashSet(r))
    
  def clearSub(e: E): Unit =
    subs.remove(e)

  private def getSub(e: E): mutable.HashSet[Reactor] = subs.getOrElseUpdate(e, mutable.HashSet.empty[Reactor])
}
