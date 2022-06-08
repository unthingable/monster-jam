package com.github.unthingable.jam.surface

import com.bitwig.extension.controller.api.HardwareLight
import com.github.unthingable.{MonsterJamExt, jam}
import com.github.unthingable.framework.HasId
import com.github.unthingable.framework.binding
import com.github.unthingable.framework.binding.{Clearable, HB, HwEvent, ButtonEvt}
import com.github.unthingable.framework.binding.HB.HBS

import scala.collection.mutable

trait SurfaceState {
  /**
   * A button and all its combo neighbors
   */
  val comboMap: mutable.Map[String, Set[Combo.JC]] = mutable.Map.empty
}

// key chords
object Combo {
  implicit private val surfaceState: SurfaceState = new SurfaceState {}

  type NamedButton = ButtonStateSupplier with HasId
  trait ComboSupplier {
    def pressed: ButtonEvt
    def releasedOne: ButtonEvt
    def releasedAll: ButtonEvt
  }

  trait HasModifier {
    val modifier: Seq[HasHwButton]
  }

  // a button combo is like a micro mode that's always on
  case class JC(b: NamedButton, mods: NamedButton*)(implicit ext: MonsterJamExt) extends Clearable {
    //val buttons: Seq[BAS] = (b1 +: bb).map(_.btn)
    //val pressedButtons = mutable.HashSet.empty[BAS]

    (b +: mods).foreach { b =>
      ext.events.addSub(b.press, _ => onPress(b))
      ext.events.addSub(b.release, _ => onRelease(b))
      // b.btn.pressedAction.addBinding(ext.a(onPress(b)))
      // b.btn.releasedAction.addBinding(ext.a(onRelease(b)))

      //surfaceState.comboMap.updateWith(b.id)(_.map(cc => cc + this))
    }

    protected[Combo] var keysOn: Int      = 0
    protected[Combo] var quasiOn: Boolean = false // flips to true when all keys pressed and to false when all released

    val pressed     = FakeAction() // all combo buttons pressed
    val releasedOne = FakeAction() // combo no longer fully held
    val releasedAll = FakeAction() // all combo buttons released

    def isPressedAll = keysOn == 1 + mods.size
    def isPressedAny = keysOn > 0
    def isModPressed = mods.exists(_.isPressed)

    private def onPress(b: NamedButton): Unit = {
      val newState = keysOn + 1
      if (newState == 1 + mods.size) {
        if (!quasiOn) {
          pressed.invoke()
          quasiOn = true
        }
      }
      keysOn = newState
    }

    private def onRelease(b: NamedButton): Unit = {
      val newState = keysOn - 1
      if (newState == mods.size) // one less than all the buttons
        releasedOne.invoke()
      if (newState == 0 && quasiOn) {
        releasedAll.invoke()
        quasiOn = false
      }
      keysOn = newState
    }

    def clear(): Unit = {
      keysOn = 0
      quasiOn = false
    }
  }


  // type HWB = ButtonLight[_] with HasId

  // implicit class BLOps[L <: HardwareLight](b: ButtonLight[L] with HasId)(implicit ext: MonsterJamExt) {
  //   def map(f: ButtonActionSupplier => ButtonActionSupplier with HasId): HWB = new ButtonLight[L] with HasId {
  //     override val light: L = b.light
  //     private val newBtn = f(btn)
  //     override def btn: ButtonActionSupplier = newBtn
  //     override val id = s"${b.id}>${newBtn.id}"
  //   }

  //   def mapB(f: ButtonLight[L] with HasId => ButtonActionSupplier with HasId): HWB = new ButtonLight[L] with HasId {
  //     override val light: L = b.light
  //     private val newBtn = f(b)
  //     override def btn: ButtonActionSupplier = newBtn
  //     override val id = s"${b.id}>${newBtn.id}"
  //   }

  //   def withNone: ButtonActionSupplier with HasId = {
  //     // all active combos involving this button
  //     def all: Seq[JC] = ext.binder.sourceMap.keys
  //       .collect {case x: JC => x}
  //       .filter(_.b == b) // only combos involving this button
  //       .toVector

  //     val ret = new ButtonActionSupplier with HasId {
  //       val id = s"${b.id}:only"

  //       override val pressed: FakeAction = FakeAction(s"$id:onlyP")

  //       override val released: FakeAction = FakeAction(s"$id:onlyR")

  //       override def isPressed: () => Boolean = () => b.btn.isPressed().get() && !all.exists(_.isModPressed)

  //       override val pressedE: ButtonEvt = ButtonEvt.Press(b.id)

  //       override val releasedE: ButtonEvt = ButtonEvt.Release(b.id)
  //     }

  //     b.btn.pressedAction.addBinding(ext.a(if (!all.exists(_.isModPressed)) ret.pressed.invoke()))
  //     b.btn.releasedAction.addBinding(ext.a(if (all.forall(!_.quasiOn)) ret.released.invoke()))

  //     ret
  //   }
  // }

  //implicit class BOps(b: ButtonActionSupplier)(implicit ext: MonsterJamExt) {
  //  type HWB = ButtonLight[_] with HasId
  //
  //
  //  def withM(mod1: HWB, modn: HWB*) = {
  //    val modifiers  = mod1 +: modn
  //    val hashString: String = modifiers.map(_.id).distinct.sorted.mkString(":")
  //
  //
  //
  //    new ButtonActionSupplier {
  //      override def pressed: HB.HBS = FakeAction(s"${b.pressed.hashCode()}:press:$hashString")
  //
  //      override def released: HB.HBS = FakeAction(s"${b.released.hashCode()}:release:$hashString")
  //
  //      override def isPressed: () => Boolean = ???
  //    }
  //  }
  //
  //  def withoutM(mod1: ButtonLight[_], modn: ButtonLight[_]*) = ???
  //
  //
  //}
}
