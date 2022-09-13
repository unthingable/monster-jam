package com.github.unthingable.framework

import scala.collection.mutable

/** Yet another eventful value thingy. Can you have too many? Apparently not.
  *
  * Let's call this an experiment.
  *
  * Allows subscribing to value changes, observer style. Somewhat duplicates EventBus functionality.
  */

class Watched[A](val init: A, val onChange: (A, A) => Unit):
  private var _value: A = init

  inline def get: A = _value

  inline def set(a: A): Unit =
    val old = _value
    _value = a
    if (old != _value) onChange(old, _value)

  inline def update(f: A => A): Unit = set(f(_value))

class Ref[A](init: A) extends Watched(init, (_, _) => ())

/** Subscribable Ref, Parameter-style.
  */
class RefSub[A](init: A) extends Ref[A](init):
  private var listeners = mutable.ListBuffer.empty[A => Unit]

  def addValueObserver(f: A => Unit): Unit =
    listeners.addOne(f)

  override val onChange: (A, A) => Unit = (_, v) => listeners.foreach(_(v))

// like Watched, but with selective notifications
abstract class RefSubSelective[A](init: A):
  type Token = Any // represents value sender/receiver
  private var value: (A, Token) = (init, null)
  protected val listeners: Iterable[(Option[Token], A => Unit)]

  inline def get: A = value._1

  inline def set(inline v: A, inline token: Token): Unit =
    val old = value
    value = (v, token)
    if (v != old._1)
      sync()
  
  inline def sync(): Unit =
      listeners.foreach((listenerToken, f) =>
        if (listenerToken.isEmpty || !listenerToken.contains(value._2)) f(value._1)
      )
