package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{BooleanHardwareProperty, HardwareAction, HardwareActionBindable, HardwareActionBinding, HardwareBindable, HardwareBinding, HardwareBindingSource, HardwareButton, InternalHardwareLightState, MultiStateHardwareLight, ObjectHardwareProperty, SettableBooleanValue}
import com.github.unthingable.MonsterJamExt

import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.{immutable, mutable}
import com.github.unthingable.jam.surface.{FakeAction, JamOnOffButton, JamRgbButton}
import ModeLayerDSL._

import java.time.{Duration, Instant}

trait Modifier
case class ModLayer(modifier: Modifier, layer: ModeActionLayer)

trait HasModLayers { def modLayers: Seq[ModLayer] }

trait Clearable {
  // stop the binding from doing its thing
  def clear(): Unit
}

sealed trait Binding[S, T, I] extends Clearable {
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

case class SupColorStateB[A <: InternalHardwareLightState](
  target: MultiStateHardwareLight, source: Supplier[A], empty: A)
  extends InBinding[Supplier[A], MultiStateHardwareLight] {
  override def bind(): Unit = target.state.setValueSupplier(source)

  override def clear(): Unit = target.state.setValueSupplier(() => empty)
}

case class SupBooleanB(target: BooleanHardwareProperty, source: BooleanSupplier)
  extends InBinding[BooleanSupplier, BooleanHardwareProperty] {
  override def bind(): Unit = target.setValueSupplier(source)

  override def clear(): Unit = target.setValueSupplier(() => false)
}


case class ObserverB[S, T, C](source: S, target: T, binder: (S, C) => Unit, receiver: C, empty: C)
  extends InBinding[S, T] {
  // observers are not removable, so
  private var action: C = empty
  binder(source, action)

  override def bind(): Unit = action = receiver

  override def clear(): Unit = action = empty
}

case class LoadActions(
  activate: HBS,
  deactivate: HBS,
)

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
  // all bindings when layer is active
  val modeBindings: Seq[Binding[_,_,_]]
  implicit val ext: MonsterJamExt

  // called when layer is activated/deactivated by the container
  def activate(): Unit = ()
  def deactivate(): Unit = ()
}

trait SelfActivatedLayer {
  // layer calls this when it wants to activate/deactivate
  val activateAction: HBS
  val deactivateAction: HBS
  // all bindings when layer is ready
  val loadBindings: Seq[Binding[_,_,_]]
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

  // activation actions invoked externally, no additional bindings to manage
  override val loadBindings: Seq[Binding[_, _, _]] = Seq.empty
}

class ModeButtonLayer(
  val name: String,
  val modeButton: JamOnOffButton,
  val modeBindings: Seq[Binding[_,_,_]] = Seq.empty,
)(implicit val ext: MonsterJamExt) extends ModeLayer with SelfActivatedLayer{
  var isPinned = true
  var isOn = false
  private var pressedAt: Instant = null

  override val activateAction: FakeAction = FakeAction(() => isOn = true)
  override val deactivateAction: FakeAction = FakeAction(() => isOn = false)

  override val loadBindings: Seq[Binding[_, _, _]] = Seq(
    SupBooleanB(modeButton.light.isOn, () => isOn),
    HB(modeButton.button.pressedAction(), () => {
      pressedAt = Instant.now()
      ext.host.println(s"$name button pressed")
      (isPinned, isOn) match {
        case (_, false) => activateAction.invoke()
        case (_, true)  => deactivateAction.invoke()
        case _          => ()
      }
    }),
    HB(modeButton.button.releasedAction(), () => {
      ext.host.println(s"$name button released")
      val operated = modeBindings.partitionMap {
        case b: HB => Left(b)
        case b => Right(b)
      }._1.exists(_.wasOperated)
      val elapsed = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
      (isPinned, operated || elapsed, isOn) match {
        case (_, true, true) => deactivateAction.invoke()
        case _               => ()
      }
    })
  )
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

object Graph {
  case class ModeNode(layer: ModeLayer) {
    protected[Graph] val parents: mutable.HashSet[ModeNode] = mutable.HashSet.empty
    protected[Graph] val children: mutable.HashSet[ModeNode] = mutable.HashSet.empty
    protected[Graph] val modeBindings: mutable.Set[Binding[_, _, _]] = mutable.Set.empty
    protected[Graph] val nodesToRestore: mutable.HashSet[ModeNode] = mutable.HashSet.empty
    protected[Graph] var isActive = false
    //override def hashCode(): Int = layer.name.hashCode()
  }

  sealed abstract class LayerGroup(val layers: Iterable[ModeLayer])
  case class Coexist(override val layers: ModeLayer*) extends LayerGroup(layers)
  case class Exclusive(override val layers: ModeLayer*) extends LayerGroup(layers)
  //case class Overlay(override val layers: ModeLayer*) extends LayerGroup(layers)

  class ModeDGraph(edges: (ModeLayer, LayerGroup)*)(implicit ext: MonsterJamExt) {

    private val layerMap: mutable.HashMap[ModeLayer, ModeNode] = mutable.HashMap.empty
    // All source elements currently bound
    private val sourceMap: mutable.HashMap[Any, Map[Binding[_, _, _], ModeNode]] = mutable.HashMap.empty


    // Assemble graph
    edges foreach { case (a, bb) =>
      bb.layers foreach { b =>
        val child = layerMap.getOrElseUpdate(b, ModeNode(b))
        val parent = layerMap.getOrElseUpdate(a, ModeNode(a))
        child.parents.add(parent)
        parent.children.add(child)
      }
    }

    // Build exclusive groups
    private val exclusiveGroups: Map[ModeNode, Set[ModeNode]] = {
      edges.map(_._2).partitionMap {
        case l: Exclusive => Left(l.layers.flatMap(layerMap.get).toSet)
        case _ => Right(())
      }._1.flatMap(s => s.map(_ -> s)).toMap
    }

    // Synthesize layer bindings
    layerMap.values.foreach { node =>
      node.modeBindings.addAll(node.layer.modeBindings ++ node.children.flatMap { child =>
        child.layer match {
          case l: SelfActivatedLayer => l.loadBindings ++ Seq(
            HB(l.activateAction, activateAction(child)),
            HB(l.deactivateAction, deactivateAction(child)),
          )
          case _ => Seq()
        }
      })
    }

    val entryNodes: Iterable[ModeNode] = layerMap.values.filter(_.parents.isEmpty)
    val exitNodes: Iterable[ModeNode] = layerMap.values.filter(_.children.isEmpty)

    // activate entry nodes
    entryNodes.foreach(activate)

    def activateAction(node: ModeNode): HardwareActionBindable = action(s"${node.layer.name} activate", () => {
      activate(node)
    })

    def deactivateAction(node: ModeNode): HardwareActionBindable = action(s"${node.layer.name} deactivate", () => {
      ext.host.println(s">> deactivate action ${node.layer.name}")

      // why is it sometimes being fired twice? no idea, but let's protect:
      if (node.isActive)
        deactivate(node)
    })

    private def activate(node: ModeNode): Unit = {
      ext.host.println(s"activating node ${node.layer.name}")
      // Deactivate exclusive
      exclusiveGroups.get(node).foreach(_.filter(_.isActive).foreach {n =>
        ext.host.println(s"exc: ${node.layer.name} deactivates ${n.layer.name}")
        n.layer match {
          case s:ModeButtonLayer => s.deactivateAction.invoke()
          case _ => deactivate(node)
        }
      })

      node.layer.activate()

      val bumpBindings: Iterable[(Binding[_, _, _], ModeNode)] = node.modeBindings
        .flatMap(b => sourceMap.get(b.surfaceElem)).flatten
      val bumpNodes: Iterable[ModeNode] = bumpBindings.map(_._2).filter(_ != node)

      if (bumpNodes.nonEmpty)
        ext.host.println(s"${node.layer.name} bumps ${bumpNodes.map(_.layer.name).mkString}")

      // remember for deactivation
      node.nodesToRestore.addAll(bumpNodes)

      // bindings within a layer are allowed to combine non-destructively, so unbind first
      bumpBindings.foreach(_._1.clear())
      node.modeBindings.foreach(_.bind())

      // one layer overrides element bindings of another, so total replacement is ok
      sourceMap.addAll(node.modeBindings
        .map((_, node))
        .groupBy(_._1.surfaceElem)
        .view.mapValues(_.toMap)
      )
      node.isActive = true
    }

    private def deactivate(node: ModeNode): Unit = {
      ext.host.println(s"deactivating node ${node.layer.name}")
      node.layer.deactivate()
      node.modeBindings.foreach { b =>
        b.clear()
        sourceMap.remove(b.surfaceElem)
      }
      // restore base
      node.nodesToRestore.foreach(activate)
      //entryNodes.foreach(activate)
      node.nodesToRestore.clear()
      node.isActive = false
    }
  }
}
