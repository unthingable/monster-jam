package com.github.unthingable.jam.surface

import com.bitwig.extension.controller.api.{HardwareActionBindable, HardwareActionBinding, HardwareBindable, HardwareBindingSource}
import com.github.unthingable.Util
import com.github.unthingable.framework.HasId

import scala.collection.mutable

/**
 * An explicitly invokable action/HardwareBindingSource. Use to construct hardware controls or as generic
 * subscribable action.
 *
 * Being a HardwareBindingSource allows this to be used anywhere a real action is required.
 *
 */
class FakeAction
  extends HardwareBindingSource[HardwareActionBinding] with Util {
  protected val invokeCallback: () => Unit = () => ()

  val callbacks = mutable.LinkedHashSet.empty[HardwareActionBindable]
  def invoke(): Unit = {
    invokeCallback()
    Array.from(callbacks).zipWithIndex.foreach { case (f, idx) =>
      //assert(idx < callbacks.size)
      //Util.println(s"calling $idx of ${callbacks.size}")
      f.invoke()
    }
  }
  def addBinding(f: HardwareActionBindable): HardwareActionBinding = {
    callbacks.addOne(f)
    () => callbacks.remove(f)
  }
  def setBinding(f: HardwareActionBindable): HardwareActionBinding = {
    callbacks.clear()
    addBinding(f)
  }
  def clearBindings(): Unit = callbacks.clear()

  override def canBindTo(o: Any): Boolean = o match {
    //case _: Runnable => true
    case _: HardwareActionBindable => true
    case _ => false
  }

  override def addBinding(h: HardwareBindable): HardwareActionBinding = h match {
    case hab: HardwareActionBindable => addBinding(hab)
    case _ => ??? // better be never
  }

  override def setBinding(h: HardwareBindable): HardwareActionBinding = h match {
    case hab: HardwareActionBindable => setBinding(hab)
    case _ => ???
  }
}

object FakeAction {
  def apply() = new FakeAction
  // when two dynamically created actions are same
  def apply(hashString: String): FakeAction = new FakeAction {
    override def hashCode(): Int = hashString.hashCode
  }
}

class FakeButton(val id: String) extends HasId {
  var isPressed: Boolean = false
  val pressedAction: FakeAction = new FakeAction { override val invokeCallback = () => isPressed = true }
  val releasedAction: FakeAction = new FakeAction { override val invokeCallback = () => isPressed = false }

  // def asHas: ButtonActionSupplier = ButtonActionSupplier(this, pressedAction, releasedAction, () => isPressed)
}
