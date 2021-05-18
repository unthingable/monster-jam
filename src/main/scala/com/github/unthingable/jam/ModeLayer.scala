package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{BooleanHardwareProperty, HardwareAction, HardwareActionBindable, HardwareActionBinding, HardwareBindable, HardwareBinding, HardwareBindingSource, HardwareButton, MultiStateHardwareLight, SettableBooleanValue}
import com.github.unthingable.MonsterJamExt

import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.mutable
import ModeLayerDSL._
import com.github.unthingable.jam.surface.FakeAction

trait Modifier
case class ModLayer(modifier: Modifier, layer: ModeActionLayer)

trait HasModLayers { def modLayers: Seq[ModLayer] }

trait Clearable {
  // stop the binding from doing its thing
  def clear(): Unit
}

sealed trait Binding[S, T, I] extends Clearable {
  //def isBound: Boolean
  def bind(): Unit
  def source: S
  def target: T

  def surfaceElem: I
}

// Controller <- Bitwig host
sealed trait InBinding[H, C] extends Binding[H, C, C] {
  def surfaceElem: C = target
}

// Controller -> Bitwig host
sealed trait OutBinding[C, H] extends Binding[C, H, C] with ModeLayerDSL {
  def surfaceElem: C = source // might be weird with observer bindings
  implicit val ext: MonsterJamExt

  // if a control was operated it's useful to know for momentary modes
  var wasOperated: Boolean = false
}


case class HB(source: HBS, target: HardwareBindable)
  (implicit val ext: MonsterJamExt)
  extends OutBinding[HBS, HardwareBindable] {
  private val bindings: mutable.ArrayDeque[HardwareBinding] = mutable.ArrayDeque.empty
  override def bind(): Unit = bindings.addAll(
    Seq(
      source.addBinding(target),
      bindOperated()
    ))

  override def clear(): Unit = {
    bindings.foreach(_.removeBinding())
    bindings.clear()
    wasOperated = false
  }

  private val operatedActions = Seq(
    action(s"", () => {wasOperated = true}),
    action(s"", _ => {wasOperated = true}),
    ext.host.createRelativeHardwareControlAdjustmentTarget(_ => {wasOperated = true})
  )

  private def bindOperated()(implicit ext: MonsterJamExt): HardwareBinding = {
    operatedActions
      .find(source.canBindTo(_))
      .map(source.addBinding).get
  }
}

object HB extends ModeLayerDSL {
  def apply(source: HBS, target: () => Unit)
    (implicit ext: MonsterJamExt): HB =
    HB(source, action("", target))
}

case class SupColorB(target: MultiStateHardwareLight, source: Supplier[Color])
  extends InBinding[Supplier[Color], MultiStateHardwareLight] {
  override def bind(): Unit = target.setColorSupplier(source)

  override def clear(): Unit = target.setColorSupplier(() => Color.nullColor())
}

case class SupBooleanB(target: BooleanHardwareProperty, source: BooleanSupplier)
  extends InBinding[BooleanSupplier, BooleanHardwareProperty] {
  override def bind(): Unit = target.setValueSupplier(source)

  override def clear(): Unit = target.setValueSupplier(() => false)
}


case class ObserverB[S, T, A](source: S, target: T, binder: (S, A => Unit) => Unit, receiver: A => Unit)
  extends InBinding[S, T] {
  // observers are not removable, so
  private var action: A => Unit = _ => ()

  override def bind(): Unit = {
    action = receiver
    binder(source, receiver)
  }

  override def clear(): Unit = action = _ => ()
}

//trait PolyAction {
//  def addBinding(h: HardwareActionBindable): Unit
//  def addBinding(f: () => Unit): Unit
//}

case class LoadActions(
  activate: HBS,
  deactivate: HBS,
  //load: Seq[_] = Seq.empty,
  //unload: Seq[_] = Seq.empty,
)

/*
Layer behavior modification strategies:
1 inherent behavior defined by layer's own bindings
2 poll: layer proactively observes modifiers value
3 push: modifier notifications are pushed to layer (redundant because can be done with 1)
4 subscribe: layer observers other modifiers value, possibly overriding modifier's behavior? (example: SOLO mode pin by SONG)

Layers and modifiers can interact in complex ways.

Layer combinator types:
- carousel: only one layer (of several) can be active at a time
- stackable: active layers can be added to and removed from the top of the stack, overriding bindings

A layer container controls how its layers combine

Panel: a group of layers for a specific area of Jam
 */

/**
 * A group of control bindings to specific host/app functions that plays well with other layers.
 *
 * A mode has two non-dormant states:
 * - ready: not active, but bindings are in place to activate
 * - active: mode layer is on
 *
 * Similarly, there are two sets of bindings:
 * - load bindings: activated when mode is placed in the ready state
 * - active bindings
 *
 * Bindings activation is managed externally by layer container.
 * @param modeBindings active bindings for this mode
 */

trait ModeLayer {
  val name: String
  val modeBindings: Seq[Binding[_,_,_]]
  implicit val ext: MonsterJamExt

  def activate(): Unit = ()
  def deactivate(): Unit = ()


  // called on layer load
  //def load(): Unit = ()
  //def unload(): Unit = ()
  // called on layer activate
  //val loadAction: HardwareActionBindable = action(s"$name load", () => load())
  //val unloadAction: HardwareActionBindable = action(s"$name unload", () => unload())
  //val activateAction: HardwareActionBindable = action(s"$name activate", () => activate())
  //val deactivateAction: HardwareActionBindable = action(s"$name deactivate", () => deactivate())
}

trait SelfActivatedLayer {
  val activateAction: HBS
  val deactivateAction: HBS
}

// does not self-activate
class SimpleModeLayer(
  val name: String,
  val modeBindings: Seq[Binding[_,_,_]] = Seq.empty,
)(implicit val ext: MonsterJamExt) extends ModeLayer


// layer activated and deactivated by distinct actions
class ModeActionLayer(
  val name: String,
  val modeBindings: Seq[Binding[_,_,_]] = Seq.empty,
  val loadActions: LoadActions //Seq[InBinding[_,_]] = Seq.empty,
)(implicit val ext: MonsterJamExt) extends ModeLayer with SelfActivatedLayer {
  override val activateAction: HBS = loadActions.activate

  override val deactivateAction: HBS = loadActions.deactivate
}

class ModeButtonLayer(
  val name: String,
  val modeButton: HardwareButton,
  val modeBindings: Seq[Binding[_,_,_]] = Seq.empty,
)(implicit val ext: MonsterJamExt) extends ModeLayer with SelfActivatedLayer{
  var isPinned = false

  override val activateAction: HBS = FakeAction()

  override val deactivateAction: HBS = FakeAction()
}

trait ModeLayerDSL {
  def action(name: String, f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => f(), () => name)

  def action(name: String, f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), () => name)

  implicit class PolyAction(a: HardwareAction)(implicit ext: MonsterJamExt) {
    def addBinding(f: () => Unit): HardwareActionBinding = a.addBinding(action("", () => f()))
  }

  // just a little bit of convenience?
  def asB(t: (BooleanHardwareProperty, SettableBooleanValue)): SupBooleanB =
    SupBooleanB.tupled(t)
  def asB(t: (MultiStateHardwareLight, Supplier[Color])): SupColorB =
    SupColorB.tupled(t)
  //def asB[A <: HardwareBindable: ClassTag](t: (HardwareBindingSource[_ <: HardwareBinding], A))(implicit ext: MonsterJamExt): HWB =
  //  HWB(t._1, t._2)
  //def asB[A <: Runnable](t: (HardwareBindingSource[_ <: HardwareBinding], A))(implicit ext: MonsterJamExt): HWB =
  //  HWB(t._1, t._2)

  type HBS = HardwareBindingSource[_ <: HardwareBinding]
}

object ModeLayerDSL extends ModeLayerDSL

trait LayerContainer {
  def select(layer: ModeActionLayer): Unit
  def pop(): Unit
}

trait Stack extends LayerContainer
trait Carousel extends LayerContainer

class LayerStack(base: ModeLayer*)(implicit ext: MonsterJamExt) extends ModeLayerDSL {
  val layers: mutable.ArrayDeque[ModeLayer] = mutable.ArrayDeque.empty
  // all source elements currently bound
  private val sourceMap: mutable.HashMap[Any, Iterable[Binding[_,_,_]]] = mutable.HashMap.empty

  //val layerMap: mutable.Map[String, ModeLayer] = mutable.Map.empty
  activateBase()

  def load(layer: ModeLayer): Unit = {
    if (layers.contains(layer)) throw new Exception(s"Layer ${layer.name} already loaded")
    layer match {
      case l: SelfActivatedLayer =>
        activate(Seq(
          HB(l.activateAction, activateAction(layer)),
          HB(l.deactivateAction, deactivateAction(layer)),
        ))
        // TODO also need to be unloaded
      case _ => ()
    }
    layers.append(layer)
  }

  def activateAction(layer: ModeLayer): HardwareActionBindable = action(s"${layer.name} activate", () => {
      activate(layer)
    })

  def deactivateAction(layer: ModeLayer): HardwareActionBindable = action(s"${layer.name} deactivate", () => {
      deactivate(layer)
    })

  def activate(layer: ModeLayer): Unit = {
    layer.activate()
    activate(layer.modeBindings)
  }


  def activateBase(): Unit = {
    base.foreach(activate)
  }

  def deactivate(layer: ModeLayer): Unit = {
    layer.deactivate()
    deactivate(layer.modeBindings)
    // restore base
    activateBase()
  }

  private def activate(bb: Iterable[Binding[_,_,_]]): Unit = {
    // bindings within a layer are allowed to combine non-destructively, so unbind first
    bb.flatMap(b => sourceMap.get(b.surfaceElem)).foreach(_.foreach(_.clear()))
    bb.foreach(_.bind())
    // one layer overrides element bindings of another, so total replacement is ok
    sourceMap.addAll(bb.groupBy(_.surfaceElem))
  }

  private def deactivate(bb: Iterable[Binding[_,_,_]]): Unit = bb.foreach { b =>
    b.clear()
    sourceMap.remove(b.surfaceElem)
  }

  //private def loadBindings(layer: ModeLayer) = {
  //  case l: ModeActionLayer => Seq(l.loadActions.activate)
  //  case l: ModeButtonLayer =>
  //}
  //
  //def pop(layer: ModeLayer): Unit = {
  //  if (layers.nonEmpty) {
  //    if (layers.last != layer) throw new Exception("Cannot pop not self")
  //    layers.removeLast()
  //    layer.outBindings.keys.foreach(_.clearBindings())
  //  }
  //  // rebind last
  //  if (layers.nonEmpty) {
  //    val layer = layers.removeLast()
  //    push(layer)
  //  }
  //}
}
