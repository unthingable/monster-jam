package com.github.unthingable.jam.surface

import com.bitwig.extension.controller.api.HardwareLight
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.binding.HB
import com.github.unthingable.jam.binding.HB.HBS

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

  type NamedButton = Button with HasId
  trait ComboSupplier {
    def pressed: HBS
    def releasedOne: HBS
    def releasedAll: HBS
  }

  // a button combo is like a micro mode that's always on
  case class JC(b1: NamedButton, bb: NamedButton*)(implicit ext: MonsterJamExt) {
    val buttons: Seq[NamedButton] = b1 +: bb

    buttons.foreach { b =>
      b.btn.pressed.addBinding(ext.a(onPress(b)))
      b.btn.released.addBinding(ext.a(onRelease(b)))
      surfaceState.comboMap.updateWith(b.id)(_.map(cc => cc + this))
    }

    private var keysOn: Int     = 0
    private var allOn : Boolean = false

    val pressed     = FakeAction() // all combo buttons pressed
    val releasedOne = FakeAction() // combo no longer fully held
    val releasedAll = FakeAction() // all combo buttons released

    val isPressedAll: () => Boolean = () => keysOn == buttons.size
    val isPressedAny: () => Boolean = () => keysOn > 0

    private def onPress(b: Button): Unit = {
      val newState = keysOn + 1
      if (newState == buttons.size) {
        if (!allOn) {
          pressed.invoke()
          allOn = true
        }
      }
      keysOn = newState
    }

    private def onRelease(b: Button): Unit = {
      val newState = keysOn - 1
      if (newState == buttons.size - 1)
        releasedOne.invoke()
      if (newState == 0 && allOn) {
        releasedAll.invoke()
        allOn = false
      }
      keysOn = newState
    }
  }

  implicit class BLOps[L <: HardwareLight](b: ButtonLight[L]) {
    def map(f: ButtonActionSupplier => ButtonActionSupplier): ButtonLight[L] = new ButtonLight[L] {
      override val light: L = b.light
      override def btn: ButtonActionSupplier = f(b.btn)
    }
  }

  implicit class BOps(b: ButtonActionSupplier) {
    type HWB = ButtonLight[_] with HasId

    def withM(mod1: HWB, modn: HWB*) = {
      val modifiers  = mod1 +: modn
      val hashString: String = modifiers.map(_.id).distinct.sorted.mkString(":")



      new ButtonActionSupplier {
        override def pressed: HB.HBS = FakeAction(s"${b.pressed.hashCode()}:press:$hashString")

        override def released: HB.HBS = FakeAction(s"${b.released.hashCode()}:release:$hashString")

        override def isPressed: () => Boolean = ???
      }
    }

    def withoutM(mod1: ButtonLight[_], modn: ButtonLight[_]*) = ???

    def withNone: ButtonActionSupplier = ???
  }
}
