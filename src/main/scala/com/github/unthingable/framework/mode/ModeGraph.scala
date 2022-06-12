package com.github.unthingable.framework.mode

import com.bitwig.extension.controller.api.HardwareActionBindable
import com.github.unthingable.framework.binding.HB.{HBS, action, isFakeAction}
import com.github.unthingable.framework.binding.{Binding, HB, BindingBehavior => BB, ModeCommand}
import com.github.unthingable.framework.mode.MultiModeLayer
import com.github.unthingable.{MonsterJamExt, Util}

import scala.collection.mutable

object Graph {
  case class ModeNode(layer: ModeLayer) {
    protected[Graph] var parent        : Option[ModeNode]              = None
    protected[Graph] val children      : mutable.HashSet[ModeNode]     = mutable.HashSet.empty
    protected[Graph] val nodeBindings  : mutable.Set[Binding[_, _, _]] = mutable.LinkedHashSet.empty
    protected[Graph] val nodesToRestore: mutable.HashSet[ModeNode]     = mutable.HashSet.empty
    protected[Graph] var isActive                                      = false
    //override def hashCode(): Int = layer.name.hashCode()
  }

  sealed abstract class LayerGroup(val layers: Iterable[ModeLayer])
  // Layers activate and deactivate as they please (the default container)
  case class Coexist(override val layers: ModeLayer*) extends LayerGroup(layers)
  // A layer deactivates all others
  case class Exclusive(override val layers: ModeLayer*) extends LayerGroup(layers)

  // Graph manages all the bindings
  class ModeDGraph(init: Seq[ModeLayer], edges: (ModeLayer, LayerGroup)*)(implicit ext: MonsterJamExt) {

    private val layerMap: mutable.Map[ModeLayer, ModeNode] = mutable.LinkedHashMap.empty

    // Assemble graph
    edges foreach { case (a, bb) =>
      bb.layers foreach { b =>
        val child  = indexLayer(b)
        val parent = indexLayer(a)

        child.parent.foreach(p => Util.println(s"${child.layer.id} already has parent ${p.layer.id}, attempting ${parent.layer.id}"))
        assert(child.parent.isEmpty || child.layer.id == "-^-")
        child.parent = Some(parent)
        parent.children.add(child)
      }
    }

    init.foreach { l =>
      Util.println(s"adding init ${l.id}")
      indexLayer(l)
    }

    layerMap.keys.collect { case x: MultiModeLayer => x }.foreach { cl =>
      val parent = layerMap.get(cl)
      cl.subModes.foreach { l =>
        Util.println(s"adding submode ${l.id}")
        val sub = indexLayer(l)
        sub.parent = parent
      }
    }

    // Build exclusive groups
    private val exclusiveGroups: Map[ModeNode, Set[ModeNode]] = {
      edges.map(_._2).partitionMap {
        case l: Exclusive => Left(l.layers.flatMap(layerMap.get).toSet)
        case _            => Right(())
      }._1.flatMap(s => s.map(_ -> s)).toMap
    }

    // Synthesize node bindings
    layerMap.values.foreach { node =>
      val bindings: Seq[Binding[_, _, _]] = node.layer.modeBindings ++ node.children.flatMap { child =>
        child.layer match {
          case l: ActivatedLayer[ModeCommand[_]] with ListeningLayer =>
            Util.println(s"${node.layer.id}: synthesizing load bindings for ${l.id}")
            l.loadBindings ++ Vector(
              HB(l.activateEvent, s"${l.id} syn act", () => activate("by action")(child)),
              HB(l.deactivateEvent, s"${l.id} syn deact", () => deactivate("by action")(child)),
            )
          case l: ActivatedLayer[ModeCommand[_]] =>
            Util.println(s"${node.layer.id}: synthesizing load bindings for ${l.id}")
            Vector(
              HB(l.activateEvent, s"${l.id} syn act", () => activate("by action")(child)),
              HB(l.deactivateEvent, s"${l.id} syn deact", () => deactivate("by action")(child)),
            )
          // case _                     => Vector.empty
        }
      } ++ (node.layer match {
        // MultiModeLayer is its submodes' parent
        case layer: MultiModeLayer => layer.subModes.flatMap { sm =>
          val smn = layerMap(sm)
          Util.println(s"${node.layer.id}: synthesizing load bindings for sub ${sm.id}")
          Vector(
            HB(sm.activateEvent, s"${node.layer.id}->${sm.id} syn act", () => activate("by action")(smn), BB(tracked = false)),
            HB(sm.deactivateEvent, s"${node.layer.id}->${sm.id} syn deact", () => deactivate("by action")(smn), BB(tracked = false)),
          )
        }
        case _                           => Vector.empty
      })
      val (managed, unmanaged) = bindings.partition(_.behavior.managed)

      // bind unmanaged now
      unmanaged.foreach(_.bind())

      // bind managed later
      node.nodeBindings.addAll(managed)
    }

    // Establish ownership by propagating owner nodes to bindings
    layerMap.values.foreach(node => node.nodeBindings.foreach(_.node = Some(node)))

    val entryNodes: Iterable[ModeNode] = layerMap.values.filter(_.parent.isEmpty)
    val exitNodes : Iterable[ModeNode] = layerMap.values.filter(_.children.isEmpty)

    ext.host.scheduleTask(() =>
    {
      // activate entry nodes
      entryNodes.foreach(activate("init entry"))

      // activate init layers
      init.flatMap(layerMap.get).foreach(activate("init"))
    }, 100)

    def indexLayer(l: ModeLayer): ModeNode = {
      // make sure we didn't reuse a layer name
      assert(!layerMap.get(l).exists(_.layer.id != l.id), s"Layer name collision: ${l.id}")
      layerMap.getOrElseUpdate(l, ModeNode(l))
    }

    def activateAction(node: ModeNode): HardwareActionBindable = action(s"${node.layer.id} activate", () => {
      activate("by action")(node)
    })

    def deactivateAction(node: ModeNode): HardwareActionBindable = action(s"${node.layer.id} deactivate", () => {
      deactivate("by action")(node)
    })

    private def activate(reason: String)(node: ModeNode): Unit = {
      Util.println(s"activating node ${node.layer.id}: $reason")

      // Deactivate exclusive
      val bumpedExc: Iterable[ModeNode] = exclusiveGroups.get(node).toVector
        //.map(_ ++ node.parents) // also deactivate parents (greedy Exclusive)
        .flatMap(_
          .filter(_.isActive)
          .filter(_ != node) // this really shouldn't happen unless the node didn't properly deactivate
          .flatMap { n =>
            Util.println(s"exc: ${node.layer.id} deactivates ${n.layer.id}")
            deactivate(s"bumped by ${node.layer.id}")(n)
            None
          })

      case class Bumped(bumper: Binding[_,_,_], bumped: Set[Binding[_,_,_]])

      val bumpBindings: Iterable[Bumped] =
        node.nodeBindings
          .filter(b => !isFakeAction(b.bindingSource) && b.behavior.exclusive)
          .map(b => Bumped(b, ext.binder.sourceMap
            .get(b.source)
            .toSet
            .flatten
            .filter(_.behavior.exclusive)
            .filter(!_.node.contains(node))))
          .filter(_.bumped.nonEmpty)
      val bumpNodes: Iterable[ModeNode] = bumpBindings.flatMap(_.bumped.flatMap(_.node)) ++ bumpedExc

      if (bumpNodes.nonEmpty) {
        def names(bb: Iterable[Binding[_,_,_]]) = bb.collect{case b: HB[_] => b.name}
        Util.println(s"${node.layer.id} bumps ${bumpNodes.map(_.layer.id).mkString(",")}: ")
        //bumpBindings.collect {case Bumped(b: HB, bb)=>(b.name, names(bb))} foreach { case (b, bb) =>
        //  ext.host.println(s" > $b <- ${bb.mkString(",")}")
        //}
      }

      // remember for deactivation
      node.nodesToRestore.addAll(bumpNodes)

      // bindings within a layer are allowed to combine non-destructively, so unbind first
      bumpBindings.flatMap(_.bumped).foreach(ext.binder.unbind)

      node.nodeBindings.foreach(ext.binder.bind)

      node.layer.onActivate()

      // one layer overrides element bindings of another, so total replacement is ok
      ext.binder.sourceMap.addAll(node.nodeBindings
        .groupBy(_.source)
        .view.mapValues(mutable.HashSet.from(_))
      )
      node.isActive = true
      Util.println(s"-- activated ${node.layer.id} ---")
    }

    private def deactivate(reason: String)(node: ModeNode): Unit = {
      Util.println(s"deactivating node ${node.layer.id}: $reason")
      node.isActive = false
      node.layer.onDeactivate()
      node.nodeBindings.foreach(ext.binder.unbind)

      def printBumpers(max: Int, cur: Int, node: ModeNode): String =
        s"${node.layer.id}" +
        (if (node.nodesToRestore.nonEmpty) " < " +
          (if (cur > max) "XXX" else node.nodesToRestore.map(printBumpers(max, cur+1, _)).mkString("[",", ","]"))
         else "")

      //Util.println(printBumpers(7, 0, node))

      for (a <- node.nodesToRestore; b <- a.nodesToRestore)
        yield Util.println(Seq(node, a, b).map(_.layer.id).mkString(" < "))

      // restore base
      node.nodesToRestore.foreach(activate(s"from bump by ${node.layer.id} <:< $reason"))

      //entryNodes.foreach(activate)
      node.nodesToRestore.clear()
      Util.println(s"-- deactivated ${node.layer.id} ---")
    }
  }
}
