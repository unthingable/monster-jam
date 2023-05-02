package com.github.unthingable.framework.mode

import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.HB.isFakeAction
import com.github.unthingable.framework.binding.ModeCommand
import com.github.unthingable.framework.binding.RestoreBehavior as RB

import scala.collection.mutable
import scala.collection.mutable.HashSet

import Util.trace

object Graph:

  case class ModeNode(layer: ModeLayer) derives CanEqual:
    protected[Graph] var parent: Option[ModeNode]                                         = None
    protected[Graph] var subParent: Option[ModeNode]                                      = None
    protected[Graph] var subAncestor: Option[ModeNode]                                    = None
    protected[Graph] val children: mutable.HashSet[ModeNode]                              = mutable.HashSet.empty
    protected[Graph] val nodeBindings: mutable.Set[Binding[?, ?, ?]]                      = mutable.LinkedHashSet.empty
    protected[Graph] val nodesToRestore: mutable.HashSet[ModeNode]                        = mutable.HashSet.empty
    protected[Graph] val bindingsToRestore: mutable.HashSet[(Binding[?, ?, ?], ModeNode)] = mutable.HashSet.empty
    protected[Graph] val bumpingMe: mutable.ArrayDeque[ModeNode]                          = mutable.ArrayDeque.empty
    protected[Graph] def isActive                                                         = layer.isOn
    // override def hashCode(): Int = layer.name.hashCode()

    // side-effect-y but convenient
    def addBinding(bb: Binding[?, ?, ?]*): Unit =
      for b <- bb do
        if b.behavior.managed then nodeBindings.add(b)
        else b.bind()

  // this first attempt at structure is now largely superceded by MultiModeLayer, but still here out of laziness
  /** Declarative ModeLayer groupings for constructing the main graph */
  sealed abstract class LayerGroup(val layers: Iterable[ModeLayer])

  /** Layers activate and deactivate as they please (the default container) */
  case class Coexist(override val layers: ModeLayer*) extends LayerGroup(layers)

  /** A layer deactivates all others */
  case class Exclusive(override val layers: ModeLayer*) extends LayerGroup(layers)

  /** A layer deactivates all others, but at least one stays on */
  case class ExclusiveOn(override val layers: ModeLayer*) extends LayerGroup(layers)

  protected enum Grouping derives CanEqual:
    case Exclusive, ExclusiveOn

  trait ModeActivator:
    protected def activate(reason: String)(node: ModeNode): Unit
    protected def deactivate(reason: String, byExternalAction: Boolean)(node: ModeNode): Unit

  trait GraphHelper:
    this: ModeActivator =>
    def synLoadBindings(node: ModeNode, cause: Option[ModeNode] = None)(using
      MonsterJamExt
    ): Seq[Binding[?, ?, ?]] =
      val layer = node.layer
      val layerBindings = layer match
        case x: ListeningLayer => x.loadBindings
        case _                 => Seq.empty
      val causeId = cause.map(c => s"${c.layer.id}->${layer.id}").getOrElse(layer.id)
      layerBindings ++
      Vector(
        EB(
          layer.selfActivateEvent,
          s"${causeId} syn act",
          () => activate(s"by ${layer.selfActivateEvent}")(node)
        ),
        EB(
          layer.selfDeactivateEvent,
          s"${causeId} syn deact",
          () => deactivate(s"by ${layer.selfDeactivateEvent}", true)(node)
        ),
      )
    end synLoadBindings
  end GraphHelper

  // Graph manages all the bindings
  class ModeDGraph(init: Seq[ModeLayer], edges: (ModeLayer, LayerGroup)*)(using ext: MonsterJamExt)
      extends ModeActivator,
        GraphHelper:

    private val layerMap: mutable.Map[ModeLayer, ModeNode] = mutable.LinkedHashMap.empty

    // Assemble graph
    edges foreach {
      case (a, bb) =>
        bb.layers foreach { b =>
          val child  = indexLayer(b)
          val parent = indexLayer(a)

          child.parent.foreach(p =>
            Util.println(
              s"${child.layer.id} already has parent ${p.layer.id}, attempting ${parent.layer.id}"
            )
          )
          assert(child.parent.isEmpty || child.layer.id == "-^-")
          child.parent = Some(parent)
          parent.children.add(child)
        }
    }

    init.foreach { l =>
      Util.println(s"adding init ${l.id}")
      indexLayer(l)
    }

    layerMap.foreach(indexSubs.tupled)

    private def indexSubs(layer: ModeLayer, node: ModeNode): Unit =
      layer match
        case mml: MultiModeLayer =>
          mml.subModes.foreach { l =>
            Util.println(s"adding submode ${l.id}")
            val sub: ModeNode = indexLayer(l)
            sub.parent = Some(node)
            sub.subParent = Some(node)
            sub.subAncestor = node.subAncestor.orElse(Some(node))
            indexSubs(l, sub)
          }
        case _ => ()

    // Build exclusive groups
    // exclusive groups cannot overlap
    private val exclusiveGroups: Map[ModeNode, (Grouping, Set[ModeNode], LayerGroup)] =
      edges
        .map(_._2)
        .collect {
          case g: (Exclusive | ExclusiveOn) =>
            val set = g.layers.flatMap(layerMap.get).toSet
            g match
              case _: Exclusive   => (Grouping.Exclusive, set, g)
              case _: ExclusiveOn => (Grouping.ExclusiveOn, set, g)
        }
        .flatMap((grouping, s, lg) => s.map(n => (n, (grouping, s, lg))))
        .toMap

    // keep track of last bumped ExclusiveOn
    private val exclusiveLastDeactivated = mutable.Map.empty[LayerGroup, ModeNode]

    // Synthesize node bindings
    layerMap.values.foreach { node =>
      Util.println(s"${node.layer.id}: synthesizing load bindings for ${node.layer.id}")
      val modeb     = node.layer.modeBindings
      val activateb = synLoadBindings(node)

      node.addBinding(modeb*)

      node.parent match
        case Some(p) => p.addBinding(activateb*)
        case None    => activateb.foreach(_.bind()) // they're unmanaged for orphan nodes
    }

    private val entryNodes: Seq[ModeNode] = layerMap.values.filter(_.parent.isEmpty).toSeq
    private val exitNodes: Seq[ModeNode]  = layerMap.values.filter(_.children.isEmpty).toSeq

    ext.host.scheduleTask(
      () =>
        // activate entry nodes - must come before init layers so that activation events are bound
        ext.events.eval("init entry")(
          entryNodes.filter(!_.isActive).flatMap(_.layer.activateEvent)*
        )

        // activate init layers
        ext.events.eval("init")(init.flatMap(layerMap.get).flatMap(_.layer.activateEvent)*)
      ,
      100
    )

    def indexLayer(l: ModeLayer): ModeNode =
      // make sure we didn't reuse a layer name
      assert(!layerMap.get(l).exists(_.layer.id != l.id), s"Layer name collision: ${l.id}")
      layerMap.getOrElseUpdate(l, ModeNode(l))

    def isOcculted(l: ModeLayer): Boolean =
      layerMap.get(l).exists(_.bumpingMe.nonEmpty)

    def reactivate(l: ModeLayer): Unit =
      layerMap
        .get(l)
        .map(node =>
          node.bumpingMe.map(n => n.subAncestor.getOrElse(n)).toSet.foreach(deactivate(s"reactivating ${l.id}", true))
          activate(s"reactivating ${l.id}")(node)
        )

    def maybeReactivate(layers: ModeLayer*): Unit =
      layers.foreach(l => if isOcculted(l) then reactivate(l))

    protected def activate(reason: String)(node: ModeNode): Unit =
      node.layer.setModeState(ModeState.Activating)
      Util.println(s"activating node ${node.layer.id}: $reason")

      // Deactivate exclusive
      val bumpedExc: Iterable[ModeNode] = exclusiveGroups
        .get(node)
        .toVector
        // .map(_ ++ node.parents) // also deactivate parents (greedy Exclusive)
        .flatMap((g, s, lg) =>
          s.filter(_.isActive)
            .filter(
              _ != node
            ) // this really shouldn't happen unless the node didn't properly deactivate
            .flatMap { n =>
              Util.println(s"exc: ${node.layer.id} deactivates ${n.layer.id}")
              val bumpMsg = s"exc bump by ${node.layer.id}"
              // Exclusive groups are still managed by hand, so
              n.layer match
                case l: HasSubModes =>
                  l.subModesToDeactivate
                    .flatMap(layerMap.get)
                    .foreach(deactivate(bumpMsg + s" (sub ${l.id})", false))
                case _ => ()
              deactivate(bumpMsg, false)(n)
              exclusiveLastDeactivated.update(lg, n)
              None
              // g match
              //   case Grouping.Exclusive   => None
              //   case Grouping.ExclusiveOn => Some(n)
            }
        )

      case class Bumped(bumper: Binding[?, ?, ?], bumped: Set[(Binding[?, ?, ?], ModeNode)])

      // useless val
      val bumpBindings: Iterable[Bumped] =
        node.nodeBindings
          // .trace("incoming bindings before filter\n", _.mkString("\n"))
          .filter(b => !isFakeAction(b.bindingSource) && b.behavior.exclusive != RB.None)
          // .trace("incoming bindings after fake filter\n", _.mkString("\n"))
          .map(b =>
            Bumped(
              b,
              ext.binder.sourceMap // assume all are active
                .get(b.source)
                .toSet
                .flatten
                // .trace(s"found in binder for $b: ")
                .filter(_._1.behavior.exclusive != RB.None)
                .filter(_._2 != node)
            )
          )
          // .trace("incoming bindings after binder filter\n", _.mkString("\n"))
          // .filter(!_.node.contains(node))))
          .filter(_.bumped.nonEmpty)

      val bumpedNodes: Iterable[ModeNode] = bumpBindings
        .filter(_._1.behavior.exclusive == RB.Layer)
        .flatMap(_.bumped.map(_._2))
        // FIXME hack: can't bump own submodes
        // .filter(!_.parent.contains(node))
        // can't bump self
        .filter(_ != node)

      // val bumpedNodes = bumpedNodesNoExc ++ bumpedExc

      val bumpedBindings = bumpBindings
        .filter(_._1.behavior.exclusive == RB.Single)
        .flatMap(_.bumped)
      // .trace("bumped bindings\n", _.mkString("\n"))

      if bumpedNodes.nonEmpty then
        def names(bb: Iterable[Binding[?, ?, ?]]) = bb.collect { case b: HB[?] => b.name }
        Util.println(s">> BUMP ${node.layer.id} bumps ${bumpedNodes.map(_.layer.id).mkString(",")}: ")

      // remember for deactivation
      node.nodesToRestore.addAll(bumpedNodes)
      node.bindingsToRestore.addAll(bumpedBindings)

      bumpedNodes.foreach(_.bumpingMe.addOne(node))

      // bindings within a layer are allowed to combine non-destructively, so unbind first
      bumpBindings.flatMap(_.bumped.map(_._1)).foreach(ext.binder.unbind)

      node.nodeBindings
        // .trace(b => if node.layer.id == "play" then s"binding $b" else "")
        .foreach(ext.binder.bind(_, node))

      node.bumpingMe.clear()
      node.layer.onActivate()

      node.layer.setModeState(ModeState.Active)
      Util.println(s"-- activated ${node.layer.id} ---")
    end activate

    protected def deactivate(reason: String, byUserAction: Boolean)(node: ModeNode): Unit =
      node.layer.setModeState(ModeState.Deactivating)
      Util.println(s"deactivating node ${node.layer.id}: $reason")
      node.layer.onDeactivate()
      node.nodeBindings.foreach(ext.binder.unbind)
      node.layer.setModeState(ModeState.Inactive)
      Util.println(s"-- deactivated ${node.layer.id} ---")

      def printBumpers(max: Int, cur: Int, node: ModeNode): String =
        s"${node.layer.id}" +
          (if node.nodesToRestore.nonEmpty then
             " < " +
             (if cur > max then "XXX"
              else node.nodesToRestore.map(printBumpers(max, cur + 1, _)).mkString("[", ", ", "]"))
           else "")

      Util.println("-- restore map:")
      Util.println(printBumpers(3, 0, node))

      // restore base
      val baseRestore = node.nodesToRestore
        // .filter(_.isActive)
        .toSeq
      val toRestore: Seq[ModeNode] = Seq(
        (baseRestore ++ baseRestore.flatMap(_.bumpingMe)).distinct.filter(n => n.isActive),
        if byUserAction then
          exclusiveGroups
            .get(node)
            .flatMap((grouping, _, lg) =>
              if grouping == Grouping.ExclusiveOn then exclusiveLastDeactivated.get(lg) else None
            )
        else Vector.empty
      ).flatten
      // maybe it's enough to simply rebind, without full on activation?
      ext.events.evalNow(s"from bump by ${node.layer.id} <:< $reason")(
        toRestore.flatMap(_.layer.activateEvent)*
      )

      // entryNodes.foreach(activate)
      baseRestore.foreach(n => n.bumpingMe.filterInPlace(_ == n))
      node.nodesToRestore.clear()

      // restore bindings after soft bump
      node.bindingsToRestore
        // .trace("restoring bindings\n", _.mkString("\n"))
        .foreach(ext.binder.bind.tupled)
      node.bindingsToRestore.clear()

      Util.println(s"-- reactivated ${node.layer.id} bumped nodes ---")
    end deactivate
  end ModeDGraph
end Graph
