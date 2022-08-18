package com.github.unthingable.framework

import com.github.unthingable.Util
import com.github.unthingable.jam.surface.WithSource

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.annotation.targetName
import scala.reflect.ClassTag

class EventBus[BaseE] {
  type Reactor[E <: BaseE] = E => Unit
  type EvtMatcher[+E <: BaseE] = E | Class[_]
  case class EvtContext[+E <: BaseE](e: E, ctx: String)

  private val valueSubs = mutable.HashMap.empty[BaseE, mutable.HashSet[Reactor[_]]]
  private val classSubs = mutable.HashMap.empty[Class[_], mutable.HashSet[Reactor[_]]]

  // private val Subs = mutable.HashMap.empty[Class[? <: BaseE], mutable.HashSet[Reactor[_]]]

  
  /* Janky actor with a queue, no mind paid to concurrency.
     Assuming the extention is single-threaded this should be fine.
   */
  private val queue              = mutable.Queue.empty[EvtContext[BaseE]]
  private var semaphore: Boolean = false

  private def trigger(): Unit =
    if !semaphore then
      semaphore = true
      evalQueue(queue)

  private def evalQueue(queue: mutable.Queue[EvtContext[BaseE]]): Unit =
    while (semaphore && queue.nonEmpty)
      val e  = queue.dequeue()
      val ev = e.e
      // Util.println(s"evt: $e")
      val receivers = receiversFor(ev) // valueSubs.get(ev).toSeq.flatten
      if (receivers.nonEmpty) Util.println(s"evt: $ev => ${receivers.size} ${e.ctx}")
      receivers.foreach(_(ev))
    semaphore = false

  def eval(context: String)(e: BaseE*): Unit =
    Util.println(s"evt: enqueueing $e with $context")
    queue.enqueueAll(e.map(EvtContext(_, context)))
    trigger()

  // jump the line
  def evalNow(context: String)(e: BaseE*): Unit =
    Util.println(s"evt: prequeueing $e with $context")
    e.foreach(ev =>
      val receivers = receiversFor(ev) // valueSubs.get(ev).toSeq.flatten
      if (receivers.nonEmpty) Util.println(s"evt: $ev => ${receivers.size} ${context}")
      receivers.foreach(_(ev))
    )

  private def eval(e: BaseE*): Unit = eval("")(e*)

  @targetName("evalS")
  private def eval(e: WithSource[BaseE, _]*): Unit = eval(e.map(_.value)*)

  @targetName("evalS")
  def eval(context: String)(e: WithSource[BaseE, _]*): Unit = eval(context)(e.map(_.value)*)

  inline def addSub[E <: BaseE](e: E, r: Reactor[E]): Unit =
    getSub(e).addOne(r)

  inline def addSub[E <: BaseE](r: Reactor[E])(using ct: ClassTag[E]): Unit =
    getSub(ct.runtimeClass).addOne(r)

  inline def rmSub[E <: BaseE](e: EvtMatcher[E], r: Reactor[E]): Unit =
    getSub(e).filterInPlace(_ != r)

  def setSub[E <: BaseE](e: E, r: Reactor[E]): Unit =
    valueSubs.update(e, mutable.HashSet(r))

  def clearSub(e: BaseE): Unit =
    valueSubs.remove(e)

  inline def getSub[E <: BaseE](e: EvtMatcher[E]): mutable.HashSet[Reactor[E]] =
    inline e match
      case ev: E => valueSubs.getOrElseUpdate(ev, mutable.HashSet.empty[Reactor[_]]).asInstanceOf[mutable.HashSet[Reactor[E]]]
      case ec: Class[_] => classSubs.getOrElseUpdate(ec, mutable.HashSet.empty[Reactor[_]]).asInstanceOf[mutable.HashSet[Reactor[E]]]

  private def receiversFor[E <: BaseE](e: E): Iterable[Reactor[E]] =
    (valueSubs.get(e).toSeq.flatten 
    ++ classSubs.get(e.getClass()).toSeq.flatten)
    .asInstanceOf[Iterable[Reactor[E]]]
}
