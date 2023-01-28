package com.github.unthingable.framework

import com.github.unthingable.Util.SelfEqual

import scala.collection.mutable

/** Yet another eventful value thingy. Can you have too many? Apparently not.
  *
  * Let's call this an experiment.
  *
  * Allows subscribing to value changes, observer style. Somewhat duplicates EventBus functionality.
  */

class Watched[A: SelfEqual](val init: A, val onChange: (A, A) => Unit):
  private var _value: A = init

  inline def get: A = _value

  inline def set(a: A): Unit =
    val old = _value
    _value = a
    if old != _value then onChange(old, _value)

  inline def update(f: A => A): Unit = set(f(_value))

class Ref[A: SelfEqual](init: A) extends Watched(init, (_, _) => ())

/** Subscribable Ref, Parameter-style.
  */
class RefSub[A: SelfEqual](init: A) extends Ref[A](init):
  private var listeners = mutable.ListBuffer.empty[A => Unit]

  def addValueObserver(f: A => Unit): Unit =
    listeners.addOne(f)

  override val onChange: (A, A) => Unit = (_, v) => listeners.foreach(_(v))

/** Like Watched, but with selective notifications */
abstract class RefSubSelective[Token, A]:
  type Listener = (Option[Token], A => Unit)
  def init: (A, Token)
  protected var value: (A, Token) = init
  protected def listeners: Iterable[Listener]

  inline def get: A = value._1

  inline def set(inline v: A, inline token: Token): Unit =
    // def set(v: A, token: Token): Unit =
    val old = value
    value = (v, token)
    // if (value._1 != old._1)
    push()

  /** Notify listeners of a value update */
  inline def push(): Unit =
    listeners.foreach((listenerToken, f) => if !listenerToken.contains(value._2) then f(value._1))

trait MultiTarget[A]:
  protected val target = mutable.ListBuffer.empty[A]
  def setTarget(v: Iterable[A]): Unit =
    target.clear()
    target.addAll(v)
  def clearTarget(): Unit = target.clear()

trait Delta[A]:
  def apply(a: A, b: A): A

case class GetSetContainer[A, B: Delta](default: B, getf: A => B, setf: (A, B, B) => Unit)

object GetSetContainer:
  def empty[A, B: Delta](default: B) = GetSetContainer[A, B](default, _ => default, (_, _, _) => ())

  given Delta[Double] with
    def apply(a: Double, b: Double) = b - a

class GetSetProxy[A, B](init: GetSetContainer[A, B])(using delta: Delta[B]) extends MultiTarget[A]:
  protected var default: B              = init.default
  protected var value: B                = init.default
  protected var getf: A => B            = init.getf
  protected var setf: (A, B, B) => Unit = init.setf

  def get: B = target.map(getf).lastOption.getOrElse(default)
  def set(v: B) =
    target.foreach(setf(_, v, delta(value, v)))
    value = v

  def update(getset: GetSetContainer[A, B]): Unit =
    value = getset.default
    getf = getset.getf
    setf = getset.setf
