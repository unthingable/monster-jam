package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.{BooleanValueChangedCallback, ColorValueChangedCallback, ValueChangedCallback}
import com.bitwig.extension.controller.api._
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.ModeLayerDSL._
import com.github.unthingable.jam.surface.{FakeAction, JamOnOffButton}

import java.time.{Duration, Instant}
import java.util.function.{BooleanSupplier, Supplier}
import scala.collection.mutable

trait Clearable {
  // stop the binding from doing its thing
  def clear(): Unit
}

sealed trait Binding[S, T, I] extends Clearable {
  def bind(): Unit
  def source: S
  def target: T

  def surfaceElem: I

  var node: Option[Graph.ModeNode] = None // backreference to the node that owns this
  def layerName: String = node.map(_.layer.name).getOrElse("")

  /**
   * Unmanaged/exclusive bindings are to be left alone when modes are removed
   */
  val managed: Boolean = true
}

// Controller <- Bitwig host
sealed trait InBinding[H, C] extends Binding[H, C, C] {
  def surfaceElem: C = target
}

// Controller -> Bitwig host
sealed trait OutBinding[C, H] extends Binding[C, H, C] with ModeLayerDSL {
  def surfaceElem: C = source // might be weird with observer bindings
  implicit val ext: MonsterJamExt

  // if a control was operated, it's useful to know for momentary modes
  var wasOperated: Boolean = false
}


case class HB(source: HBS, target: HardwareBindable,
  tracked: Boolean = true,
  override val managed: Boolean = true)
  (implicit val ext: MonsterJamExt)
  extends OutBinding[HBS, HardwareBindable] {

  private val bindings: mutable.ArrayDeque[HardwareBinding] = mutable.ArrayDeque.empty

  override def bind(): Unit = bindings.addAll(
    Seq(
      source.addBinding(target)
    ) ++ (if (tracked)
            operatedActions
              .find(source.canBindTo)
              .map(source.addBinding)
          else Seq.empty)
  )

  override def clear(): Unit = {
    bindings.foreach(_.removeBinding())
    bindings.clear()
    source.clearBindings() // one of these is probably unnecessary
    wasOperated = false
  }

  private val operatedActions = Seq(
    action(() => s"$layerName: HB: unit operated", () => {wasOperated = true}),
    action(() => s"$layerName: HB: double operated", _ => {wasOperated = true}),
    ext.host.createRelativeHardwareControlAdjustmentTarget(_ => {wasOperated = true})
  )
}

object HB extends ModeLayerDSL {
  def apply(source: HBS, name: String, target: () => Unit)
    (implicit ext: MonsterJamExt): HB =
    HB(source, action(name, target))
  def apply(source: HBS, name: String, target: () => Unit, tracked: Boolean)
    (implicit ext: MonsterJamExt): HB =
    HB(source, action(name, target), tracked = tracked)
  def apply(source: HBS, name: String, target: () => Unit, tracked: Boolean, managed: Boolean)
    (implicit ext: MonsterJamExt): HB =
    HB(source, action(name, target), tracked = tracked, managed = managed)
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


class ObserverB[S, T, C](val source: S, val target: T, binder: (S, C) => Unit, receiver: C, empty: C)
  extends InBinding[S, T] {
  // observers are not removable, so
  private var action: C = empty
  binder(source, action)

  override def bind(): Unit = action = receiver

  override def clear(): Unit = action = empty
}

// not working?
case class ValObserverB[A <: ValueChangedCallback, T](value: Value[A], receiver: A, override val target: T)
  (implicit emp: EmptyCB[A])
extends ObserverB[Value[A], T, A](value, target, _.addValueObserver(_), receiver, emp.empty)

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
 * - active (mode) bindings
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
  //def activate(): Unit = ()
  //def deactivate(): Unit = ()
}

/**
 * Layer whose (de)activation is controlled by actions
 */
trait ActivatedLayer[+A <: HBS] {
  val activateAction  : A
  val deactivateAction: A
}

/**
 * (De)activation is triggered by external actions
 */
trait ExtActivatedLayer extends ActivatedLayer[HBS]

/**
 * (De)activation is triggered by internal actions: must invoke them explicitly
 */
trait IntActivatedLayer extends ActivatedLayer[FakeAction]

trait ListeningLayer {
  // all bindings when layer is ready and listening
  val loadBindings: Seq[Binding[_,_,_]]
}

// does not self-activate
abstract class SimpleModeLayer(
  val name: String
)(implicit val ext: MonsterJamExt) extends ModeLayer {}

object SimpleModeLayer {
  def apply(name: String, modeBindings: Seq[Binding[_, _, _]])
    (implicit ext: MonsterJamExt): SimpleModeLayer = {
    val x = modeBindings
    new SimpleModeLayer(name) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
    }
  }
}


// layer activated and deactivated by distinct externally invoked actions
abstract class ModeActionLayer(
  val name: String,
  val loadActions: LoadActions //Seq[InBinding[_,_]] = Seq.empty,
)(implicit val ext: MonsterJamExt) extends ModeLayer with ExtActivatedLayer {
  override val activateAction: HBS = loadActions.activate
  override val deactivateAction: HBS = loadActions.deactivate

  //// activation actions invoked externally, no additional bindings to manage
  //override final val loadBindings: Seq[Binding[_, _, _]] = Seq.empty
}

//object ModeActionLayer {
//  def apply(name: String, modeBindings: Seq[Binding[_, _, _]], loadActions: LoadActions)
//    (implicit ext: MonsterJamExt): ModeActionLayer = {
//    val x = modeBindings
//    new ModeActionLayer(name, loadActions) {
//      override val modeBindings: Seq[Binding[_, _, _]] = x
//    }
//  }
//}

sealed trait GateMode
object GateMode {
  case object Gate extends GateMode
  case object Toggle extends GateMode
  case object Auto extends GateMode
}

abstract class ModeButtonLayer(
  val name: String,
  val modeButton: JamOnOffButton,
  val gateMode: GateMode = GateMode.Auto
)(implicit val ext: MonsterJamExt) extends ModeLayer with IntActivatedLayer with ListeningLayer {
  var isOn = false
  private var pressedAt: Instant = null

  // For MBL (de)activateActions are internal, safe to call
  override final val activateAction: FakeAction = FakeAction(() => isOn = true)
  override final val deactivateAction: FakeAction = FakeAction(() => isOn = false)

  private lazy val hBindings: Seq[HB] = modeBindings.partitionMap {
    case b: HB => Left(b)
    case b     => Right(b)
  }._1

  override final val loadBindings: Seq[Binding[_, _, _]] = Seq(
    SupBooleanB(modeButton.light.isOn, () => isOn),
    HB(modeButton.button.pressedAction, s"$name: mode button pressed", () => {
      pressedAt = Instant.now()
      if (isOn) {
        // this press is only captured when the mode is still active
        deactivateAction.invoke()
      } else
        activateAction.invoke()
    },
      tracked = false),
    HB(modeButton.button.releasedAction, s"$name: mode button released", () => gateMode match {
      case GateMode.Gate   => if (isOn) deactivateAction.invoke()
      case GateMode.Toggle => ()
      case GateMode.Auto   =>
        if (isOn) {
          val operated = hBindings.exists(_.wasOperated)
          val elapsed  = Instant.now().isAfter(pressedAt.plus(Duration.ofSeconds(1)))
          if (operated || elapsed)
            deactivateAction.invoke()
        }
    },
      tracked = false)
  )
}

object ModeButtonLayer {
  def apply(name: String, modeButton: JamOnOffButton, modeBindings: Seq[Binding[_, _, _]], gateMode: GateMode = GateMode.Auto)
    (implicit ext: MonsterJamExt): ModeButtonLayer = {
    val x = modeBindings
    new ModeButtonLayer(name, modeButton, gateMode) {
      override val modeBindings: Seq[Binding[_, _, _]] = x
    }
  }
}

trait ModeLayerDSL {
  def action(name: String, f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => {
      ext.host.println(s"! $name")
      f()
    }, () => name)

  def action(name: Supplier[String], f: () => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(() => {
      ext.host.println(s"! ${name.get()}")
      f()
    }, name)

  def action(name: String, f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), () => name)

  def action(name: Supplier[String], f: Double => Unit)(implicit ext: MonsterJamExt): HardwareActionBindable =
    ext.host.createAction(f(_), name)

  //implicit class PolyAction(a: HardwareAction)(implicit ext: MonsterJamExt) {
  //  def addBinding(f: () => Unit): HardwareActionBinding = a.addBinding(action("", () => f()))
  //}

  type HBS = HardwareBindingSource[_ <: HardwareBinding]

  trait EmptyCB[A <: ValueChangedCallback] { def empty: A}

  implicit object emptyBool extends EmptyCB[BooleanValueChangedCallback] {
    override def empty: BooleanValueChangedCallback = _ => ()
  }

  implicit object emptyColor extends EmptyCB[ColorValueChangedCallback] {
    override def empty: ColorValueChangedCallback = (_,_,_) => ()
  }
}

object ModeLayerDSL extends ModeLayerDSL

object Graph {
  case class ModeNode(layer: ModeLayer) {
    protected[Graph] var parent: Option[ModeNode] = None
    protected[Graph] val children: mutable.HashSet[ModeNode] = mutable.HashSet.empty
    protected[Graph] val nodeBindings: mutable.Set[Binding[_, _, _]] = mutable.HashSet.empty
    protected[Graph] val nodesToRestore: mutable.HashSet[ModeNode] = mutable.HashSet.empty
    protected[Graph] var isActive = false
    //override def hashCode(): Int = layer.name.hashCode()
  }

  sealed abstract class LayerGroup(val layers: Iterable[ModeLayer])
  // Layers activate and deactivate as they please
  case class Coexist(override val layers: ModeLayer*) extends LayerGroup(layers)
  // A layer deactivates all others
  case class Exclusive(override val layers: ModeLayer*) extends LayerGroup(layers)
  // Repeated activation cycles layers (?)
  case class Cycle(override val layers: ModeLayer*) extends LayerGroup(layers)
  //case class Overlay(override val layers: ModeLayer*) extends LayerGroup(layers)

  // Graph manages all the bindings
  class ModeDGraph(edges: (ModeLayer, LayerGroup)*)(implicit ext: MonsterJamExt) {

    private val layerMap: mutable.HashMap[ModeLayer, ModeNode] = mutable.HashMap.empty
    // All source elements currently bound by us
    private val sourceMap: mutable.HashMap[Any, mutable.HashSet[Binding[_, _, _]]] = mutable.HashMap.empty


    // Assemble graph
    edges foreach { case (a, bb) =>
      bb.layers foreach { b =>
        val child  = layerMap.getOrElseUpdate(b, ModeNode(b))
        val parent = layerMap.getOrElseUpdate(a, ModeNode(a))

        child.parent.foreach(p => ext.host.println(s"${child.layer.name} already has parent ${p.layer.name}, attempting ${parent.layer.name}"))
        assert(child.parent.isEmpty || child.layer.name == "-^-")
        child.parent = Some(parent)
        //child.parents.add(parent)

        parent.children.add(child)
      }
    }

    // Build exclusive groups
    private val exclusiveGroups: Map[ModeNode, Set[ModeNode]] = {
      edges.map(_._2).partitionMap {
        case l: Exclusive => Left(l.layers.flatMap(layerMap.get).toSet)
        case _            => Right(())
      }._1.flatMap(s => s.map(_ -> s)).toMap
    }

    // Synthesize layer bindings
    layerMap.values.foreach { node =>
      val bindings = node.layer.modeBindings ++ node.children.flatMap { child =>
        child.layer match {
          case l: ActivatedLayer[HBS] with ListeningLayer =>
            ext.host.println("synthesizing load bindings for " + l.name)
            l.loadBindings ++ Seq(
              HB(l.activateAction, activateAction(child)),
              HB(l.deactivateAction, deactivateAction(child)),
          )
          case l: ActivatedLayer[HBS] =>
            ext.host.println("synthesizing load bindings for " + l.name)
            Seq(
              HB(l.activateAction, activateAction(child)),
              HB(l.deactivateAction, deactivateAction(child)),
            )
          case _                     => Seq()
        }
      }
      val (managed, unmanaged) = bindings.partition(_.managed)

      // bind unmanaged now
      unmanaged.foreach(_.bind())

      // bind managed later
      node.nodeBindings.addAll(managed)
    }

    // Establish ownership by propagating owner nodes to bindings
    layerMap.values.foreach(node => node.nodeBindings.foreach(_.node = Some(node)))

    val entryNodes: Iterable[ModeNode] = layerMap.values.filter(_.parent.isEmpty)
    val exitNodes : Iterable[ModeNode] = layerMap.values.filter(_.children.isEmpty)

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
      exclusiveGroups.get(node)
        //.map(_ ++ node.parents) // also deactivate parents (greedy Exclusive)
        .foreach(_
          .filter(_.isActive)
          .filter(_ != node) // this really shouldn't happen unless the node didn't properly deactivate
          .foreach { n =>
            ext.host.println(s"exc: ${node.layer.name} deactivates ${n.layer.name}")
            n.layer match {
              case s: IntActivatedLayer => s.deactivateAction.invoke()
              case _                    => deactivate(n)
            }
      })

      //node.layer.activate()

      val bumpBindings: Iterable[Binding[_, _, _]] = node.nodeBindings
        .flatMap(b => sourceMap.get(b.surfaceElem)).flatten //.filter(_.node.get != node)
      val bumpNodes: Iterable[ModeNode] = bumpBindings.flatMap(_.node).filter(_ != node)

      if (bumpNodes.nonEmpty)
        ext.host.println(s"${node.layer.name} bumps ${bumpNodes.map(_.layer.name).mkString}")

      // node stays active?
      //bumpNodes.foreach(deactivate)

      // remember for deactivation
      node.nodesToRestore.addAll(bumpNodes)

      // bindings within a layer are allowed to combine non-destructively, so unbind first
      bumpBindings.foreach({ b =>
        //ext.host.println(s"${node.layer.name}: clearing binding for: ${b.layerName}")
        //b.clear()
        unbind(b)
      })
      node.nodeBindings.foreach(bind)

      // one layer overrides element bindings of another, so total replacement is ok
      sourceMap.addAll(node.nodeBindings
        .groupBy(_.surfaceElem)
        .view.mapValues(mutable.HashSet.from(_))
      )
      node.isActive = true
      ext.host.println(s"-- activated ${node.layer.name} ---")
    }

    private def deactivate(node: ModeNode): Unit = {
      ext.host.println(s"deactivating node ${node.layer.name}")
      //node.layer.deactivate()
      node.nodeBindings.foreach(unbind)

      // restore base
      node.nodesToRestore.foreach(activate)

      //entryNodes.foreach(activate)
      node.nodesToRestore.clear()
      node.isActive = false
      ext.host.println(s"-- deactivated ${node.layer.name} ---")
    }

    def bind(binding: Binding[_,_,_]) = {
      binding.bind()
      sourceMap.getOrElseUpdate(binding.surfaceElem, mutable.HashSet.empty).add(binding)
    }

    def unbind(binding: Binding[_,_,_]) = {
      //ext.host.println(s"unbinding for: ${binding.layerName}")
      binding.clear()
      sourceMap.get(binding.surfaceElem).foreach(_.remove(binding))
    }
  }
}
