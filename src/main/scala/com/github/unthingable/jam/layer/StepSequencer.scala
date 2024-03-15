package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.BooleanValue
import com.bitwig.extension.controller.api.Clip
import com.bitwig.extension.controller.api.ClipLauncherSlot
import com.bitwig.extension.controller.api.ClipLauncherSlotBank
import com.bitwig.extension.controller.api.Track
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.Watched
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.BindingDSL
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.GlobalEvent
import com.github.unthingable.framework.binding.GlobalEvent.SlotSelected
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.JCB
import com.github.unthingable.framework.binding.ModeCommand
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.SupColorB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.ListeningLayer
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.ModeState
import com.github.unthingable.framework.mode.MultiModeLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.TrackId
import com.github.unthingable.jam.stepSequencer.*
import com.github.unthingable.jam.stepSequencer.mode.*
import com.github.unthingable.jam.stepSequencer.state.*
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamRgbButton
import com.github.unthingable.jam.surface.KeyMaster.JC

import scala.collection.mutable

import Util.{trace, delay}

trait StepSequencer extends BindingDSL:
  this: Jam =>
  val stepModeMap = Map(
    0 -> StepMode.One,
    1 -> StepMode.Two,
    // 1 -> StepMode.OneFull,
    2 -> StepMode.Four,
    3 -> StepMode.Eight
  )

  // Large submodes live in separate files, small ones live here
  trait StepModes extends TrackedState, ModeLayer, StepMatrix, VelNote, NoteParam

  object stepSequencer
      extends ModeButtonLayer("STEP", j.step, GateMode.AutoInverse),
        MultiModeLayer,
        ListeningLayer,
        TrackedState(selectedClipTrack),
        StepCap,
        StepModes:
    // a mirror of the bitwig clip, channel / x / y

    // val steps =
    //   ArraySeq.fill(16)(ArraySeq.fill(gridWidth)(ArraySeq.fill(gridHeight)(null: NoteStep)))

    // clip.addNoteStepObserver(ns => steps(ns.channel())(ns.x()).update(ns.y(), ns))

    // devices.itemCount().addValueObserver(v => Util.println(v.toString))
    // clip.getPlayStop.addValueObserver(v => Util.println(s"beats $v"))
    // clip.playingStep().addValueObserver(v => Util.println(s"playing step $v"))

    // follow track selection
    // ext.cursorTrack
    //   .position()
    //   .addValueObserver(v =>
    //     selectedClipTrack.selectChannel(ext.cursorTrack)
    //     // updateState(ext.cursorTrack)
    //     ext.host.scheduleTask(() => updateState(ext.cursorTrack), 30)
    //   )

    // follow clip selection
    ext.events.addSub((e: SlotSelected) =>
      Util.println(s"received $e")
      if isOn then selectedClipTrack.selectChannel(superBank.getItemAt(e.globalTrack))
      localState.selectedClips.update(e.globalTrack, e.globalSlot)
    )

    Vector(
      clip.clipLauncherSlot().isPlaying(),
      clip.clipLauncherSlot().hasContent(),
      secondClip.exists,
      secondClip.clipLauncherSlot.sceneIndex,
      clip.exists,
      clip.getPlayStart,
      clip.getPlayStop,
      clip.getAccent,
      clip.getShuffle,
      clip.getLoopStart,
      clip.getLoopLength,
      clip.playingStep,
      clip.color,
      clip.canScrollKeysDown,
      clip.canScrollKeysUp,
      clip.canScrollStepsBackwards,
      clip.canScrollStepsForwards,
      selectedClipTrack.color,
      selectedClipTrack.position,
      selectedClipTrack.name,
      ext.transport.isPlaying,
      devices.itemCount(), // hopefully this gets updated
    ).foreach(_.markInterested())

    // page follower
    clip
      .playingStep()
      .addValueObserver(step =>
        if isOn && ext.transport.isPlaying().get() &&
          ext.preferences.stepFollow
            .get() && !localState.stepState.get.steps.nonEmpty
        then
          val currentPage: Int = ts.stepScrollOffset / ts.stepPageSize
          val playingPage: Int = step / ts.stepPageSize
          if currentPage != playingPage then setStepPage(playingPage)
      )

    lazy val stepPages = SimpleModeLayer(
      "stepPages",
      j.sceneButtons.zipWithIndex.flatMap { (btn, i) =>
        inline def hasContent  = clip.getLoopLength().get() > currentBank * 8 + i * ts.stepSize * ts.stepPageSize
        inline def currentPage = ts.stepScrollOffset / ts.stepPageSize
        inline def currentBank = currentPage / 8
        Vector(
          EB(btn.st.press, "", () => if hasContent then setStepPage(i + currentBank * 8)),
          SupColorStateB(
            btn.light,
            () =>
              if hasContent then
                if ext.transport.isPlaying().get() && clip.clipLauncherSlot().isPlaying().get() &&
                  clip
                    .playingStep()
                    .get() / ts.stepPageSize == i
                then colorManager.stepScene.playing
                else if i == currentPage % 8 then colorManager.stepScene.selected
                else colorManager.stepScene.nonEmpty
              else colorManager.stepScene.empty
          ),
        )
      }
    )

    lazy val stepShiftPages = ModeButtonLayer(
      "stepShiftPages",
      j.Mod.Shift,
      j.sceneButtons.zipWithIndex.flatMap { (btn, i) =>
        inline def hasContent  = clip.getLoopLength().get() > i * ts.stepSize * ts.stepPageSize * 8
        inline def currentPage = ts.stepScrollOffset / ts.stepPageSize
        inline def currentBank = currentPage / 8

        Vector(
          EB(btn.st.press, "", () => if hasContent then setStepPage(i * 8)),
          SupColorStateB(
            btn.light,
            () =>
              if hasContent then
                if ext.transport.isPlaying().get() && clip.playingStep().get() / (ts.stepPageSize * 8) == i then
                  colorManager.stepScene.playing
                else if i == currentBank then colorManager.stepScene.selected
                else colorManager.stepScene.nonEmpty
              else colorManager.stepScene.empty
          ),
        )
      } ++
        // workaround to keep SHIFT button doing what it does, because as submode it will bump SHIFT bindings for bottom
        Vector(shiftMatrix, shiftTransport).map(_.modeBindings).flatten,
      GateMode.Gate
    )
    end stepShiftPages

    // Circuit-like note mode
    // lazy val noteMatrix = new SimpleModeLayer("noteMatrix") {

    // }

    inline def scrollEnc(dir: UpDown): Unit =
      if j.encoder.push.isPressed().get() then scrollXBy(dir, 1)
      else scrollYBy(dir, 1)

    lazy val stepEnc = SimpleModeLayer(
      "stepEnc",
      Vector(
        HB(
          j.encoder.turn,
          "note scroll",
          stepTarget(
            () => scrollEnc(UpDown.Down),
            () => scrollEnc(UpDown.Up)
          )
        )
      )
    )

    lazy val patLength = new SimpleModeLayer("patLength"):
      override def modeBindings: Seq[Binding[?, ?, ?]] = (0 until 64).flatMap { idx =>
        JCB(
          this.id,
          j.matrix(idx / 8)(idx % 8),
          () =>
            Util.println(s"set playStop $idx")
            val newPatLength = idx + 1
            clip.getLoopLength.set(newPatLength.toDouble)
            clip.getPlayStop.set(newPatLength.toDouble) // doesn't follow the first length change
          ,
          () => (),
          () =>
            if clip.getPlayStop.get() > idx then JamColorState(clipColor, 2)
            else JamColorState.empty
        )
      } ++ Vector(SupBooleanB(j.solo.light, () => true))

    lazy val gridSelect = ModeButtonLayer(
      "gridSelect",
      j.grid,
      j.sceneButtons.zipWithIndex.flatMap { (btn, idx) =>
        stepModeMap.get(idx) match
          case None =>
            Vector(
              EB(btn.st.press, "", Noop),
              SupColorStateB(btn.light, () => JamColorState.empty)
            )
          case Some(sm) =>
            Vector(
              EB(btn.st.press, "", () => setGrid(sm)),
              SupColorStateB(
                btn.light,
                () =>
                  if ts.stepMode == sm then JamColorState(Color.whiteColor(), 3)
                  else JamColorState(clipColor, 2)
              )
            )
      } ++
        Vector(
          HB(j.encoder.touch.pressedAction, "", () => incStepSize(0)),
          HB(j.encoder.turn, "", stepTarget(() => incStepSize(1), () => incStepSize(-1)))
        ),
    )

    def setChannel(ch: Int) =
      ext.host.showPopupNotification(s"Step sequencer: MIDI channel ${ch + 1}")
      setState(ts.copy(channel = ch))

    lazy val chanSelect = new ModeButtonLayer(
      "chanSelect",
      j.perform,
      GateMode.Gate
    ):
      val chanBindings: Seq[Binding[?, ?, ?]] =
        (for (row <- 0 until 4; col <- 0 until 4) yield
          val btn  = j.matrix(row + 4)(col + 4)
          val chan = (3 - row) * 4 + col
          Vector(
            SupColorB(btn.light, () => if chan == ts.channel then Color.whiteColor else clipColor),
            EB(btn.st.press, s"select channel $chan", () => setChannel(chan))
          )
        ).flatten

      // note buttons
      val noteMatrix = Vector(
        Vector(0, 2, 4, 0, 7, 9, 11, 0),
        Vector(1, 3, 5, 6, 8, 10, 12, 0),
      )

      val rootBindings =
        (for (row <- 0 to 1; col <- EIGHT) yield
          val btn     = j.matrix(row)(col)
          val noteIdx = (noteMatrix(row)(col) - 1).asInstanceOf[RealNote]
          if noteIdx == -1.asInstanceOf[RealNote] then
            Vector(
              SupColorStateB(btn.light, () => JamColorState.empty),
              EB(btn.st.press, "", () => ())
            )
          else
            Vector(
              SupColorStateB(
                btn.light,
                () =>
                  JamColorState(
                    if ts.scaleRoot == noteIdx then JamColorBase.BLUE else JamColorBase.CYAN,
                    2
                  )
              ),
              EB(btn.st.press, "", () => setState(ts.copy(scaleRoot = noteIdx)))
            )
        ).flatten

      val scaleBindings =
        (for (row <- 2 to 3; col <- EIGHT) yield
          val btn      = j.matrix(row)(col)
          val scaleIdx = (row - 2) * 8 + col
          Vector(
            SupColorStateB(
              btn.light,
              () =>
                JamColorState(
                  JamColorBase.FUCHSIA,
                  if ts.scaleIdx == scaleIdx then 3 else 1
                )
            ),
            EB(btn.st.press, "", () => setState(ts.copy(scaleIdx = scaleIdx)))
          )
        ).flatten

      override def modeBindings = chanBindings ++ rootBindings ++ scaleBindings

    lazy val dpadStep = SimpleModeLayer(
      "dpadStep",
      Vector(
        EB(j.dpad.up.st.press, "scroll page up", () => scrollYPage(UpDown.Up)),
        EB(j.dpad.down.st.press, "scroll page down", () => scrollYPage(UpDown.Down)),
        EB(j.dpad.left.st.press, "scroll step left", () => scrollXBy(-1)),
        EB(j.dpad.right.st.press, "scroll step right", () => scrollXBy(1)),
        SupBooleanB(j.dpad.up.light, () => canScrollY(UpDown.Up)),
        SupBooleanB(j.dpad.down.light, () => canScrollY(UpDown.Down)),
        SupBooleanB(j.dpad.left.light, clip.canScrollStepsBackwards()),
        SupBooleanB(j.dpad.right.light, clip.canScrollStepsForwards()),
      )
    )

    override lazy val stepMain = SimpleModeLayer(
      "stepMain",
      Vector(
        EB(j.Combo.Shift.notes.press, "", () => ext.preferences.altNoteRow.toggle())
      )
    )

    lazy val stepRegular = MultiModeLayer(
      "stepRegular",
      Vector(
        stepMatrix,
        stepEnc,
        velAndNote,
        notePages,
      )
    )

    override val subModes: Vector[ModeLayer] = Vector(
      stepMain,
      stepPages,
      stepGate,
      stepShiftPages,
      stepRegular,
      patLength,
      gridSelect,
      chanSelect,
      dpadStep,
      noteParam,
    )

    override def onStepState(from: StepState, to: StepState): Unit =
      val stateDiff = Util.comparator(from, to) andThen (_.unary_!)

      if to.steps.nonEmpty then
        // delay because it's a more pleasant experience when buttons aren't flashing all the time during regular edits
        delay(
          const.stepLongHold,
          if localState.stepState.get.steps.nonEmpty && !noteParam.isOn then
            ext.events.eval("steps selected")(noteParam.activateEvent*)
        )
        // if !noteParam.isOn then ext.events.eval("steps selected")(noteParam.activateEvent*)
      else if to.steps.isEmpty && noteParam.isOn then ext.events.eval("steps unselected")(noteParam.deactivateEvent*)

      if to.steps.nonEmpty && stateDiff(_.steps) then
        // wait for NoteParam to turn on and then update
        delay(const.stepLongHold + 10, if localState.stepState.get.steps.nonEmpty then noteParam.setCurrentSteps())
    end onStepState

    override def subModesToActivate =
      (Vector(stepRegular, stepMatrix, stepPages, stepEnc, dpadStep, stepMain) ++
        (subModes :+ velAndNote).filter(m => m.isOn || (m == velAndNote && ts.noteVelVisible))).distinct

    override val modeBindings: Seq[Binding[?, ?, ?]] =
      Vector(
        EB(j.Combo.Shift.solo.press, "shift-solo pressed", () => patLength.activateEvent),
        EB(j.Combo.Shift.solo.releaseAll, "shift-solo released", () => patLength.deactivateEvent),
      ) ++ JCB.empty(j.song)

    // TODO refactor this already
    // Find the first existing clip on a track
    def findClip(): Option[Clip] =
      secondClip.selectFirst()
      var cnt = 0
      while cnt < 64 && !secondClip.exists().get() do
        cnt += 1
        secondClip.selectNext()
      if secondClip.exists().get() then Some(secondClip) else None

    def newClip(trackNum: Int, slotNum: Int): Unit =
      val track: Track                = trackBank.getItemAt(trackNum)
      val slots: ClipLauncherSlotBank = track.clipLauncherSlotBank()
      val slot: ClipLauncherSlot      = slots.getItemAt(slotNum)
      track.createNewLauncherClip(slotNum)
      slots.select(slotNum)
      if ext.transport.isPlaying().get() then slot.launchWithOptions("default", "continue_with_quantization")

    override def onActivate(): Unit =
      // restoreState()
      // updateCurrentState()
      clip.setStepSize(ts.stepSize)
      fineClip.setStepSize(ts.stepSize / fineRes.toDouble)

      clip.scrollToStep(ts.stepScrollOffset)
      fineClip.scrollToStep(ts.stepScrollOffset * fineRes)

      super.onActivate()

      if selectedClipTrack.position().get() < 0 then selectedClipTrack.selectFirst()

      ext.host.scheduleTask(
        () =>
          if !clip.exists().get() && selectedClipTrack.position().get() >= 0 then
            findClip() match
              case None =>
                val t = selectedClipTrack.position().get()
                val c = localState.selectedClips.getOrElse(t, 0)
                newClip(t, c)
              case Some(foundClip) =>
                clip.selectClip(foundClip)
                clip.clipLauncherSlot().select()
        ,
        30
      )
    end onActivate

    override def onSeqState(oldSt: SeqState, newSt: SeqState): Unit =
      val stateDiff = Util.comparator(oldSt, newSt) andThen (_.unary_!)

      if modeState._1 == ModeState.Active then
        (newSt.noteVelVisible, velAndNote.isOn) match
          case (true, false) => ext.events.eval("sync track SeqState")(velAndNote.activateEvent*)
          case (false, true) => ext.events.eval("sync track SeqState")(velAndNote.deactivateEvent*)
          case _             => ()

      if stateDiff(_.stepSize) then
        clip.setStepSize(newSt.stepSize)
        fineClip.setStepSize(newSt.stepSize / fineRes.toDouble)

      if stateDiff(_.stepScrollOffset) then
        clip.scrollToStep(newSt.stepScrollOffset)
        fineClip.scrollToStep(newSt.stepScrollOffset * fineRes)

    override lazy val extraOperated = stepGate.modeBindings

    /** Clip selector, page follow, CLEAR/DUPLICATE
      *
      * Activated by stepGateActivator.
      */
    lazy val stepGate = SimpleModeLayer(
      "stepGate",
      Vector(
        EB(j.clear.st.press, "clear steps", () => stepSequencer.clip.clearSteps(), BB.soft),
        EB(j.duplicate.st.press, "duplicate pattern", () => stepSequencer.clip.duplicateContent(), BB.soft),
        EB(j.play.st.press, "toggle pattern follow", () => ext.preferences.stepFollow.toggle(), BB.soft),
        SupBooleanB(j.clear.light, () => true, BB.soft),
        SupBooleanB(j.duplicate.light, () => true, BB.soft),
        SupBooleanB(
          j.play.light,
          () =>
            if ext.preferences.stepFollow.get() then j.Mod.blink3
            else !ext.transport.isPlaying().get(),
          BB.soft
        )
      ) ++
        (for row <- EIGHT; col <- EIGHT yield
          val btn: JamRgbButton        = j.matrix(row)(col)
          val target: ClipLauncherSlot = trackBank.getItemAt(col).clipLauncherSlotBank().getItemAt(row)
          val clipEq: BooleanValue     = clip.clipLauncherSlot().createEqualsValue(target)
          clipEq.markInterested()
          target.isPlaying().markInterested()
          target.hasContent().markInterested()
          target.exists().markInterested()
          Vector(
            EB(
              btn.st.press,
              "",
              () =>
                if !target.hasContent().get then newClip(col, row)
                else target.select()
            ),
            SupColorStateB(
              btn.light,
              () =>
                if clipEq.get() then JamColorState(JamColorBase.WHITE, 3)
                else if clip.clipLauncherSlot().hasContent.get() then
                  JamColorState(target.color().get(), if target.isPlaying().get() then 3 else 1)
                else JamColorState.empty,
              behavior = BB.soft
            ),
          )
        ).flatten ++
        Vector(
          EB(j.dpad.left.st.press, "", () => (), BB.soft),
          EB(j.dpad.right.st.press, "", () => (), BB.soft),
          EB(j.dpad.up.st.press, "", () => (), BB.soft),
          EB(j.dpad.down.st.press, "", () => (), BB.soft)
        ),
    )
  end stepSequencer

  /** Sidecar mode to coexist with stepSequencer layer, activates stepGate
    *
    * Easier to separate them like this because this both clobbers the mode button and bumps submodes.
    */
  object stepGateActivator
      extends ModeButtonLayer("stepGateActivator", j.step, gateMode = GateMode.Gate, silent = true):
    override val modeBindings: Seq[Binding[?, ?, ?]] = Vector()

    override def onActivate(): Unit =
      super.onActivate()
      ext.events.eval("stepGate activator")(stepSequencer.stepGate.activateEvent*)

    override def onDeactivate(): Unit =
      ext.events.eval("stepGate deactivator")(stepSequencer.stepGate.deactivateEvent*)
      super.onDeactivate()

end StepSequencer

/* todos and ideas
[x] store grid settings per track
[ ] autoscroll to content when there isn't any
[x] knob scrolls notes
[ ] note pages
- step-hold to select or create clips before entering sequencer
- adjust mode (by holding aux? tune?)
- reactivate velAndNote
vel
release vel
vel spread
(start)
duration
pan
timbre
pressure

chance
occurrence
recurrence length
recurrence mask
repeat count
repeat rate
repeat curve
repeat vel end

mute

- select clips by holding STEP

problems:
  deactivating STEP with TUNE should reactivate previous slider mode
    - multibutton cycle mode? or exclusive reactivator?
  dirty TUNE tracking
 */
