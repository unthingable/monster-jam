package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{Bank, Clip}
import com.github.unthingable.ShowHide
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.{Binding, CycleMode, GateMode, HB, IntActivatedLayer, Jam, ModeButtonLayer, ModeCycleLayer, SimpleModeLayer, SupColorStateB}
import com.github.unthingable.jam.surface.{JamColorState, JamRgbButton}

trait Shift { this: Jam =>
  /**
   * Shift matrix row
   */
  lazy val shiftMatrix = new ModeButtonLayer("shiftMatrix", j.Modifiers.Shift, GateMode.Gate) {
    val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
    override val modeBindings: Seq[Binding[_, _, _]] =
      (Vector(
        (JAMColorBase.RED, () => ext.application.undo()),
        (JAMColorBase.GREEN, () => ext.application.redo()),
        (JAMColorBase.LIME, () => clip.quantize(1.0)),
        (JAMColorBase.LIME, () => clip.quantize(0.5)),
        (JAMColorBase.MAGENTA, () => clip.transpose(-1)),
        (JAMColorBase.MAGENTA, () => clip.transpose(1)),
        (JAMColorBase.FUCHSIA, () => clip.transpose(-12)),
        (JAMColorBase.FUCHSIA, () => clip.transpose(12)),
      ).zipWithIndex.flatMap { case ((color, action), idx) =>
        val button = j.matrix(0)(idx)
        Vector(HB(button.pressedAction, s"shift-$idx matrix pressed", action)) ++ (
          if (ext.preferences.shiftRow.get())
            Vector(SupColorStateB(
              button.light, () => JamColorState(
                color,
                brightness = if (button.isPressed()) 2 else 0),
              JamColorState.empty))
          else Vector.empty
          )
      }
       ++ (if (ext.preferences.shiftRow.get()) Vector(
        SupColorStateB(j.matrix(1)(0).light, () =>
          if (ShowHide.withName(ext.docPrefs.hideDisabled.get()) == ShowHide.Hide)
            JamColorState(JAMColorBase.RED, 0)
          else JamColorState(JAMColorBase.YELLOW, 0)
        )) else Vector.empty)
       ++ Vector(
        HB(j.matrix(1)(0).pressedAction, "toggle hide disabled", () => {
          if (ShowHide.withName(ext.docPrefs.hideDisabled.get()) == ShowHide.Hide)
            ext.docPrefs.hideDisabled.set(ShowHide.Show.toString)
          else ext.docPrefs.hideDisabled.set(ShowHide.Hide.toString)
        })
      ))
  }

  lazy val shiftPages = new ModeCycleLayer("shiftMatrix", j.Modifiers.Shift, CycleMode.Gate) {
    trackBank.itemCount().markInterested()
    trackBank.scrollPosition().markInterested()
    sceneBank.itemCount().markInterested()
    sceneBank.scrollPosition().markInterested()
    ext.preferences.shiftGroup.markInterested()

    def bankB(b: Bank[_], btn: Int => JamRgbButton, idx: Int) = Vector(
      SupColorStateB(btn(idx).light, () =>
        if (b.itemCount().get() > idx * 8)
          if (((idx * 8) - 7 until (idx * 8) + 7) contains b.scrollPosition().get())
            JamColorState(JAMColorBase.WHITE, 2)
          else
            JamColorState(JAMColorBase.WARM_YELLOW, 0)
        else JamColorState.empty
        , JamColorState.empty),
      HB(btn(idx).pressedAction, "shift-scroll page $idx", () => b.scrollPosition().set(idx * 8))
    )

    val trackPages: Vector[Binding[_, _, _]] =
      if (ext.preferences.shiftGroup.get())
        EIGHT.flatMap(bankB(trackBank, j.groupButtons, _))
      else
        Vector.empty

    override val subModes = Vector(
      new SimpleModeLayer("scene pages") with IntActivatedLayer {
        override val modeBindings: Vector[Binding[_, _, _]] =
          EIGHT.flatMap(bankB(sceneBank, j.sceneButtons, _)) ++ trackPages
      },
      new SimpleModeLayer("superscene pages") with IntActivatedLayer {
        override val modeBindings: Vector[Binding[_, _, _]] = {
          trackPages ++
          EIGHT.flatMap(idx => bankB(sceneBank, j.matrix(7), idx) ++ Vector(
            SupColorStateB(j.sceneButtons(idx).light, () =>
              if (idx == superScene.pageIndex)
                JamColorState(JAMColorBase.WHITE, 3)
              else if (superScene.lastScene.exists(i => EIGHT.map(_ + (idx * 8)).contains(i)))
                     JamColorState(JAMColorBase.LIME, 0)
                   else if (superScene.page(idx).exists(_.nonEmpty))
                          JamColorState(JAMColorBase.ORANGE, 0)
                        else JamColorState.empty
              , JamColorState.empty),
            HB(j.sceneButtons(idx).pressedAction, "super scene page $idx", () => superScene.pageIndex = idx)
          ))
        }
      }
    )

    override def activate(): Unit = {
      selected = if (superScene.isOn) Some(1) else Some(0)
      super.activate()
    }
  }

  lazy val globalShift = new ModeButtonLayer("globalShift", j.Modifiers.Shift, GateMode.Gate) {
    val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
    override val modeBindings: Seq[Binding[_, _, _]] = Vector(
      HB(j.duplicate.pressedAction, "shift dup clip content", () => clip.duplicateContent())
    )
  }

}