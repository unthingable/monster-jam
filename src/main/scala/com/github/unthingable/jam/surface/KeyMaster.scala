package com.github.unthingable.jam.surface

import com.bitwig.extension.controller.api.HardwareLight
import com.github.unthingable.{jam, MonsterJamExt}
import com.github.unthingable.framework.HasId
import com.github.unthingable.framework.binding
import com.github.unthingable.framework.binding.{ButtonEvt, Clearable, Event, HB, HwEvent}
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

  enum RawButtonEvent:
    case Press, Release

  enum ComboEvent extends HwEvent:
    case Pressed(buttonId: String)     // all combo buttons pressed
    case ReleasedOne(buttonId: String) // combo no longer fully held
    case ReleasedAll(buttonId: String) // all combo buttons released

  enum KeyType:
    case Modifier, Normal

  case class JC(b: NamedButton, mods: NamedButton*)(using ext: MonsterJamExt)
      extends Clearable,
        HasId {
    override val id = b.id + "<" + mods.map(_.id).mkString("+")

    val allb: Seq[NamedButton] = b +: mods
    val allbIds                = allb.map(_.id)

    /** Tell the world what this key is to us
      */
    inline def lookup(buttonId: String): Option[KeyType] =
      if buttonId == b.id then Some(KeyType.Normal)
      else if mods.exists(_.id == buttonId) then Some(KeyType.Modifier)
      else None

    protected[KeyMaster] var keysOn: Int = 0
    // flips to true when all keys pressed and to false when all released
    protected[KeyMaster] var quasiOn: Boolean = false

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
      // this would happen if a modifier was used as a mode button
      if keysOn <= 0 then Util.println(s"WARNING JC $id binding activated externally")

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

  def eval(buttonId: String, ev: RawButtonEvent)(using ext: MonsterJamExt): Iterable[Event] =
    Util.println(s"KeyMaster eval $buttonId $ev")
    val (jcs, lookup) = ext.binder.sourceMap.keys
      .collect { case x: JC => x }
      .flatMap(x => x.lookup(buttonId).map((x, _)))
      .unzip // only combos involving this button
    val comboEvents: Iterable[ComboEvent] = jcs
      .flatMap(jc =>
        ev match
          case RawButtonEvent.Press   => jc.onPress(buttonId)
          case RawButtonEvent.Release => jc.onRelease(buttonId)
      )
    /* Compromise: let modifier keys through */
    comboEvents ++ (if comboEvents.isEmpty || lookup.toSeq.contains(KeyType.Modifier)
                    then
                      Seq(ev match
                        case RawButtonEvent.Press   => ButtonEvt.Press(buttonId)
                        case RawButtonEvent.Release => ButtonEvt.Release(buttonId)
                      )
                    else Seq.empty)
}
