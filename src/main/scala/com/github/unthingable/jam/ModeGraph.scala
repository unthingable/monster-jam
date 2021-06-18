package com.github.unthingable.jam

import com.bitwig.extension.controller.api.HardwareActionBindable
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.BindingDSL.{HBS, action, isFakeAction}

import scala.collection.mutable

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
  // Layers activate and deactivate as they please (the default container)
  case class Coexist(override val layers: ModeLayer*) extends LayerGroup(layers)
  // A layer deactivates all others
  case class Exclusive(override val layers: ModeLayer*) extends LayerGroup(layers)

  // Graph manages all the bindings
  class ModeDGraph(init: Seq[ModeLayer], edges: (ModeLayer, LayerGroup)*)(implicit ext: MonsterJamExt) {

    private val layerMap: mutable.HashMap[ModeLayer, ModeNode] = mutable.HashMap.empty
    // All source elements currently bound by us
    private val sourceMap: mutable.HashMap[Any, mutable.HashSet[Binding[_, _, _]]] = mutable.HashMap.empty

    // Assemble graph
    edges foreach { case (a, bb) =>
      bb.layers foreach { b =>
        val child  = indexLayer(b)
        val parent = indexLayer(a)

        child.parent.foreach(p => ext.host.println(s"${child.layer.name} already has parent ${p.layer.name}, attempting ${parent.layer.name}"))
        assert(child.parent.isEmpty || child.layer.name == "-^-")
        child.parent = Some(parent)
        parent.children.add(child)
      }
    }

    init.foreach { l =>
      ext.host.println(s"adding init ${l.name}")
      indexLayer(l)
    }

    layerMap.keys.collect { case x: ModeCycleLayer => x }.foreach { cl =>
      val parent = layerMap.get(cl)
      cl.subModes.foreach { l =>
        ext.host.println(s"adding submode ${l.name}")
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
      val bindings = node.layer.modeBindings ++ node.children.flatMap { child =>
        child.layer match {
          case l: ActivatedLayer[HBS] with ListeningLayer =>
            ext.host.println(s"${node.layer.name}: synthesizing load bindings for ${l.name}")
            l.loadBindings ++ Seq(
              HB(l.activateAction, s"${l.name} syn act", activateAction(child)),
              HB(l.deactivateAction, s"${l.name} syn deact", deactivateAction(child)),
            )
          case l: ActivatedLayer[HBS] =>
            ext.host.println(s"${node.layer.name}: synthesizing load bindings for ${l.name}")
            Seq(
              HB(l.activateAction, s"${l.name} syn act", activateAction(child)),
              HB(l.deactivateAction, s"${l.name} syn deact", deactivateAction(child)),
            )
          case _                     => Seq()
        }
      } ++ (node.layer match {
        // ModeCycleLayer is its submodes' parent
        case layer: ModeCycleLayer => layer.subModes.flatMap { sm =>
          val smn = layerMap(sm)
          ext.host.println(s"${node.layer.name}: synthesizing load bindings for sub ${sm.name}")
          Seq(
            HB(sm.activateAction, s"${node.layer.name}->${sm.name} syn act", activateAction(smn), tracked = false),
            HB(sm.deactivateAction, s"${node.layer.name}->${sm.name} syn deact", deactivateAction(smn), tracked = false),
          )
        }
        case _                     => Seq()
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
      entryNodes.foreach(activate)

      // activate init layers
      init.flatMap(layerMap.get).foreach(activate)
    }, 100)

    def indexLayer(l: ModeLayer): ModeNode = {
      // make sure we didn't reuse a layer name
      assert(!layerMap.get(l).exists(_.layer.name != l.name), s"Layer name collision: ${l.name}")
      layerMap.getOrElseUpdate(l, ModeNode(l))
    }

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
            deactivate(n)
            //n.layer match {
            //  case s: IntActivatedLayer => s.deactivateAction.invoke()
            //  case _                    => deactivate(n)
            //}
          })

      case class Bumped(bumper: Binding[_,_,_], bumped: Set[Binding[_,_,_]])

      val bumpBindings: Iterable[Bumped] =
        node.nodeBindings
          .filter(b => !isFakeAction(b.surfaceElem) && b.behavior.exclusive)
          .map(b => Bumped(b, sourceMap
            .get(b.surfaceElem)
            .toSet
            .flatten
            .filter(_.behavior.exclusive)
            .filter(!_.node.contains(node))))
          .filter(_.bumped.nonEmpty)
      val bumpNodes: Iterable[ModeNode] = bumpBindings.flatMap(_.bumped.flatMap(_.node))

      if (bumpNodes.nonEmpty) {
        def names(bb: Iterable[Binding[_,_,_]]) = bb.collect{case b: HB => b.name}
        ext.host.println(s"${node.layer.name} bumps ${bumpNodes.map(_.layer.name).mkString}: ")
        bumpBindings.collect {case Bumped(b: HB, bb)=>(b.name, names(bb))} foreach { case (b, bb) =>
          ext.host.println(s" > $b <- ${bb.mkString(",")}")
        }
      }

      // node stays active?
      //bumpNodes.foreach(deactivate)

      // remember for deactivation
      node.nodesToRestore.addAll(bumpNodes)

      // bindings within a layer are allowed to combine non-destructively, so unbind first
      bumpBindings.flatMap(_.bumped).foreach(unbind)

      node.nodeBindings.foreach(bind)

      node.layer.activate()

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
      node.layer.deactivate()
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
