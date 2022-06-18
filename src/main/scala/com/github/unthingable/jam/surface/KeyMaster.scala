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

// key chords
object KeyMaster {
  type NamedButton = HasButtonState & HasId & HasButton[_]

  trait HasModifier {
    val modifier: Seq[HasHwButton]
  }

  enum ComboEvent extends HwEvent:
    case Pressed(buttonId: String) // all combo buttons pressed
    case ReleasedOne(buttonId: String) // combo no longer fully held
    case ReleasedAll(buttonId: String) // all combo buttons released

  case class JC(b: NamedButton, mods: NamedButton*)(using ext: MonsterJamExt) extends Clearable, HasId {
    override val id = b.id + "<" + mods.map(_.id).mkString("+")

    val allb: Seq[NamedButton] = b +: mods
    val allbIds = allb.map(_.id)

    protected[KeyMaster] var keysOn: Int      = 0
    protected[KeyMaster] var quasiOn: Boolean = false // flips to true when all keys pressed and to false when all released

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
      } else None

    private def onRelease(): Option[ComboEvent] = 
      val newState = keysOn - 1
      keysOn = newState
      if (newState > 0 && newState <= mods.size && quasiOn) // one less than all the buttons
        Some(releaseOne.value)
      else if (newState == 0 && quasiOn) {
        quasiOn = false
        Some(releaseAll.value)
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
    Util.println(ret.toString())
    if ret.nonEmpty then ret else Seq(ev match
      case RawButtonEvent.Press => ButtonEvt.Press(buttonId)
      case RawButtonEvent.Release => ButtonEvt.Release(buttonId)
    )
}
