package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{Bank, Clip}
import com.github.unthingable.JamSettings.ShowHide
import com.github.unthingable.framework.mode.{
  CycleMode,
  GateMode,
  ModeButtonCycleLayer,
  ModeButtonLayer,
  SimpleModeLayer
}
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.{JamColorState, JamRgbButton}
import com.github.unthingable.jam.*
import com.github.unthingable.framework.binding.{Binding, EB, SupColorStateB}
import com.github.unthingable.framework.binding.EB

trait Shift:
  this: Jam with SceneL with StepSequencer =>

  /** Shift matrix row
    */
  lazy val shiftMatrix = new ModeButtonLayer("shiftMatrix", j.Mod.Shift, GateMode.Gate):
    val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
    override val modeBindings: Seq[Binding[?, ?, ?]] =
      (Vector(
        (JamColorBase.RED, () => ext.application.undo()),
        (JamColorBase.GREEN, () => ext.application.redo()),
        (JamColorBase.LIME, () => clip.quantize(1.0)),
        (JamColorBase.LIME, () => clip.quantize(0.5)),
        (JamColorBase.MAGENTA, () => clip.transpose(-1)),
        (JamColorBase.MAGENTA, () => clip.transpose(1)),
        (JamColorBase.FUCHSIA, () => clip.transpose(-12)),
        (JamColorBase.FUCHSIA, () => clip.transpose(12)),
      ).zipWithIndex.flatMap {
        case ((color, action), idx) =>
          val button = j.matrix(0)(idx)
          Vector(EB(button.st.press, s"shift-$idx matrix pressed", action)) ++ (
            if ext.preferences.shiftRow.get() then
              Vector(
                SupColorStateB(
                  button.light,
                  () => JamColorState(color, brightness = if button.btn.isPressed().get then 2 else 0),
                  JamColorState.empty
                )
              )
            else Vector.empty
          )
      }
        ++ (if ext.preferences.shiftRow.get() then
              Vector(
                SupColorStateB(
                  j.matrix(1)(0).light,
                  () =>
                    if ext.docPrefs.hideDisabled.get() == ShowHide.Hide then JamColorState(JamColorBase.RED, 0)
                    else JamColorState(JamColorBase.YELLOW, 0)
                )
              )
            else Vector.empty)
        ++ Vector(
          EB(
            j.matrix(1)(0).st.press,
            "toggle hide disabled",
            () =>
              if ext.docPrefs.hideDisabled.get() == ShowHide.Hide then ext.docPrefs.hideDisabled.set(ShowHide.Show)
              else ext.docPrefs.hideDisabled.set(ShowHide.Hide)
          ),
        ))

  lazy val shiftPages = new ModeButtonCycleLayer("shiftPages", j.Mod.Shift, CycleMode.Gate):
    trackBank.itemCount().markInterested()
    trackBank.scrollPosition().markInterested()
    sceneBank.itemCount().markInterested()
    sceneBank.scrollPosition().markInterested()
    ext.preferences.shiftGroup.markInterested()

    def bankB(b: Bank[?], btn: Int => JamRgbButton, idx: Int) = Vector(
      SupColorStateB(
        btn(idx).light,
        () =>
          if b.itemCount().get() > idx * 8 then
            if ((idx * 8) - 7 until (idx * 8) + 7) contains b.scrollPosition().get() then
              JamColorState(JamColorBase.WHITE, 2)
            else JamColorState(JamColorBase.WARM_YELLOW, 0)
          else JamColorState.empty,
        JamColorState.empty
      ),
      EB(btn(idx).st.press, "shift-scroll page $idx", () => b.scrollPosition().set(idx * 8))
    )

    val trackPages: Vector[Binding[?, ?, ?]] =
      if ext.preferences.shiftGroup.get() then EIGHT.flatMap(bankB(trackBank, j.groupButtons, _))
      else Vector.empty

    override val subModes = Vector(
      new SimpleModeLayer("scenePagesSub"):
        override val modeBindings: Vector[Binding[?, ?, ?]] =
          EIGHT.flatMap(bankB(sceneBank, j.sceneButtons, _)) ++ trackPages
      ,
      new SimpleModeLayer("superscenePagesSub"):
        override val modeBindings: Vector[Binding[?, ?, ?]] =
          trackPages ++
            EIGHT.flatMap(idx =>
              bankB(sceneBank, j.matrix(7), idx) ++ Vector(
                SupColorStateB(
                  j.sceneButtons(idx).light,
                  () =>
                    if idx == superSceneSub.pageIndex then JamColorState(JamColorBase.WHITE, 3)
                    else if superSceneSub.lastScene.exists(i => EIGHT.map(_ + (idx * 8)).contains(i)) then
                      JamColorState(JamColorBase.LIME, 0)
                    else if superSceneSub.page(idx).exists(_.nonEmpty) then JamColorState(JamColorBase.ORANGE, 0)
                    else JamColorState.empty,
                  JamColorState.empty
                ),
                EB(j.sceneButtons(idx).st.press, "super scene page $idx", () => superSceneSub.pageIndex = idx)
              )
            )
    )

    override def onActivate(): Unit =
      // selected = if (superSceneSub.isOn) Some(1) else Some(0)
      super.onActivate()
      select(if superSceneSub.isOn then 1 else 0)
end Shift
