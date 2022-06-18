package com.github.unthingable.jam.surface

import com.bitwig.extension.controller.api.HardwareLight
import com.github.unthingable.{MonsterJamExt, jam}
import com.github.unthingable.framework.HasId
import com.github.unthingable.framework.binding
import com.github.unthingable.framework.binding.{Clearable, HB, HwEvent, ButtonEvt, Event}
import com.github.unthingable.framework.binding.HB.HBS

import scala.collection.mutable
import com.bitwig.`extension`.controller.api.HardwareButton
import com.github.unthingable.Util

// trait SurfaceState {
//   /**
//    * A button and all its combo neighbors
//    */
//   val comboMap: mutable.Map[String, Set[KeyMaster.JC]] = mutable.Map.empty
// }

// key chords
object KeyMaster {
  // implicit private val surfaceState: SurfaceState = new SurfaceState {}

  type NamedButton = HasButtonState & HasId & HasButton[_]

  // trait ComboSupplier {
  //   def press: ComboEvent
  //   def releaseOne: ComboEvent
  //   def releaseAll: ComboEvent
  // }

  trait HasModifier {
    val modifier: Seq[HasHwButton]
  }

  enum ComboEvent extends HwEvent:
    case Pressed(buttonId: String) // all combo buttons pressed
    case ReleasedOne(buttonId: String) // combo no longer fully held
    case ReleasedAll(buttonId: String) // all combo buttons released

  case class JC(b: NamedButton, mods: NamedButton*)(using ext: MonsterJamExt) extends Clearable, HasId {
    //val buttons: Seq[BAS] = (b1 +: bb).map(_.btn)
    //val pressedButtons = mutable.HashSet.empty[BAS]

    override val id = b.id + "<" + mods.map(_.id).mkString("+")

    // import reflect.Selectable.reflectiveSelectable

    val allb: Seq[NamedButton] = b +: mods
    val allbIds = allb.map(_.id)

    // allb.foreach { b =>
    //   // ext.events.addSub(b.press, _ => onPress(b))
    //   // ext.events.addSub(b.release, _ => onRelease(b))
    //   // b.st.pressedAction.addBinding(ext.a(onPress(b)))
    //   // b.st.releasedAction.addBinding(ext.a(onRelease(b)))

    //   //surfaceState.comboMap.updateWith(b.id)(_.map(cc => cc + this))
    // }

    protected[KeyMaster] var keysOn: Int      = 0
    protected[KeyMaster] var quasiOn: Boolean = false // flips to true when all keys pressed and to false when all released

    // val pressed     = FakeAction() // all combo buttons pressed
    // val releasedOne = FakeAction() // combo no longer fully held
    // val releasedAll = FakeAction() // all combo buttons released

    val press      = WithSource(ComboEvent.Pressed(id), this)
    val releaseOne = WithSource(ComboEvent.ReleasedOne(id), this)
    val releaseAll = WithSource(ComboEvent.ReleasedAll(id), this)

    inline def isPressedAll = allb.forall(_.st.isPressed)
    inline def isPressedAny = allb.exists(_.st.isPressed)
    inline def isModPressed = mods.exists(_.st.isPressed)

    def onPress(bid: String): Option[ComboEvent] =
      Option.when(allbIds.contains(bid))(onPress()).flatten
    
    def onRelease(bid: String): Option[ComboEvent] = 
      Option.when(allbIds.contains(bid))(onRelease()).flatten

    private def onPress(): Option[ComboEvent] = 
      val newState = keysOn + 1
      keysOn = newState
      if (newState == 1 + mods.size) {
          quasiOn = true
          Some(press.value)
          // ext.events.eval(press)
          // pressed.invoke()
      } else None

    private def onRelease(): Option[ComboEvent] = 
      val newState = keysOn - 1
      keysOn = newState
      if (newState > 0 && newState <= mods.size) // one less than all the buttons
        Some(releaseOne.value)
        // ext.events.eval(releaseOne)
        // releasedOne.invoke()
      else if (newState == 0) {
        quasiOn = false
        Some(releaseAll.value)
        // ext.events.eval(releaseAll)
        // releasedAll.invoke()
      } else None

    def clear(): Unit = 
      keysOn = 0
      quasiOn = false
  }

  enum RawButtonEvent:
    case Press, Release

  def eval(buttonId: String, ev: RawButtonEvent)(using ext: MonsterJamExt): Seq[Event] =
    // Util.println(s"KeyMaster eval $buttonId $ev")
    val ret = ext.binder.sourceMap.keys
    .collect {case x: JC => x}
      .filter(_.allbIds.contains(buttonId)) // only combos involving this button
      .flatMap(jc =>
        ev match
          case RawButtonEvent.Press => jc.onPress(buttonId)
          case RawButtonEvent.Release => jc.onRelease(buttonId)
      ).toSeq
    if ret.nonEmpty then ret else Seq(ev match
      case RawButtonEvent.Press => ButtonEvt.Press(buttonId)
      case RawButtonEvent.Release => ButtonEvt.Release(buttonId)
    )


  // true if no currently active combo is using this button
  // inline def checkCombo(buttonId: String)(using ext: MonsterJamExt): Boolean =
  //   ext.binder.sourceMap.keys
  //   .collect {case x: JC => x}
  //     .filter(_.allb.exists(_.id == buttonId)) // only combos involving this button
  //     .exists(_.isPressedAny)

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
