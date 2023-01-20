package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.Clip
import com.bitwig.extension.controller.api.CursorTrack
import com.bitwig.extension.controller.api.Device
import com.bitwig.extension.controller.api.DeviceBank
import com.bitwig.extension.controller.api.NoteOccurrence
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State as NSState
import com.bitwig.extension.controller.api.Parameter
import com.bitwig.extension.controller.api.PinnableCursorClip
import com.bitwig.extension.controller.api.Setting
import com.bitwig.extension.controller.api.Track
import com.github.unthingable.JamSettings.DpadScroll
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.GetSetProxy
import com.github.unthingable.framework.Watched
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.BindingDSL
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.GlobalEvent
import com.github.unthingable.framework.binding.GlobalEvent.ClipSelected
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.JCB
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.SupColorB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.CycleMode
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.ListeningLayer
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.ModeState
import com.github.unthingable.framework.mode.MultiModeLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.framework.quant
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.JamParameter
import com.github.unthingable.jam.SliderBankMode
import com.github.unthingable.jam.SliderOp
import com.github.unthingable.jam.TrackId
import com.github.unthingable.jam.TrackId.apply
import com.github.unthingable.jam.TrackTracker
import com.github.unthingable.jam.stepSequencer.*
import com.github.unthingable.jam.stepSequencer.mode.*
import com.github.unthingable.jam.stepSequencer.state.*
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.KeyMaster.JC

import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.ArraySeq

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
      extends ModeButtonLayer("STEP", j.step),
        MultiModeLayer,
        ListeningLayer,
        TrackedState(selectedClipTrack),
        StepCap,
        StepModes:
    // a mirror of the bitwig clip, channel / x / y

    // val steps =
    //   ArraySeq.fill(16)(ArraySeq.fill(gridWidth)(ArraySeq.fill(gridHeight)(null: NoteStep)))

    // clip.addNoteStepObserver(ns => steps(ns.channel())(ns.x()).update(ns.y(), ns))

    devices.itemCount().addValueObserver(v => Util.println(v.toString))
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
    ext.events.addSub((e: ClipSelected) =>
      Util.println(s"received $e")
      // if (isOn)
      // selectedClipTrack.selectChannel(superBank.getItemAt(e.globalTrack))
      localState.selectedClips.update(e.globalTrack, e.globalClip)
    )

    Vector(
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

    clip.setStepSize(ts.stepSize)
    clip.scrollToStep(ts.stepScrollOffset)
    fineClip.setStepSize(ts.stepSize / fineRes.toDouble)

    // def detectDrum(): Option[Device] = (0 until devices.itemCount().get()).map(devices.getDevice).find(_.hasDrumPads.get())

    // page follower
    clip
      .playingStep()
      .addValueObserver(step =>
        if isOn && ext.transport.isPlaying().get() && ext.preferences.stepFollow.get() then
          val currentPage: Int = ts.stepScrollOffset / ts.stepPageSize
          val playingPage: Int = step / ts.stepPageSize
          if currentPage != playingPage then setStepPage(playingPage)
      )

    lazy val stepPages = SimpleModeLayer(
      "stepPages",
      j.sceneButtons.zipWithIndex.flatMap { (btn, i) =>
        def hasContent = clip.getLoopLength().get() > i * ts.stepSize * ts.stepPageSize
        Vector(
          EB(btn.st.press, "", () => if hasContent then setStepPage(i)),
          SupColorStateB(
            btn.light,
            () =>
              if hasContent then
                if ext.transport.isPlaying().get() && clip.playingStep().get() / ts.stepPageSize == i then
                  colorManager.stepScene.playing
                else if i == ts.stepScrollOffset / ts.stepPageSize then colorManager.stepScene.selected
                else colorManager.stepScene.nonEmpty
              else colorManager.stepScene.empty
          ),
        )
      }
    )

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
      } ++ Vector(
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

    lazy val stepGate = ModeButtonLayer(
      "stepGate",
      j.step,
      Vector(
        EB(j.clear.st.press, "clear steps", () => clip.clearSteps(), BB.soft),
        EB(j.duplicate.st.press, "duplicate pattern", () => clip.duplicateContent(), BB.soft),
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
      ),
      gateMode = GateMode.Gate,
      silent = true
    )

    override val subModes: Vector[ModeLayer] = Vector(
      stepMain,
      stepPages,
      stepRegular,
      patLength,
      gridSelect,
      chanSelect,
      dpadStep,
      tune,
      stepGate,
    )

    override def onStepState(from: StepState, to: StepState): Unit =
      val stateDiff = Util.comparator(from, to) andThen (_.unary_!)
      if tune.isOn && stateDiff(_.steps.map(_.step)) then tune.setCurrentSteps(to.steps.map(_.step))
      // if stateDiff(_.scaleIdx) || stateDiff(_.scaleRoot) then
      //   ???

    override def subModesToActivate =
      (Vector(stepRegular, stepMatrix, stepPages, stepEnc, dpadStep, stepMain) ++ (subModes :+ velAndNote).filter(m =>
        m.isOn || (m == velAndNote && ts.noteVelVisible)
      )).distinct

    override val modeBindings: Seq[Binding[?, ?, ?]] =
      Vector(
        EB(j.Combo.Shift.solo.press, "shift-solo pressed", () => patLength.activateEvent),
        EB(j.Combo.Shift.solo.releaseAll, "shift-solo released", () => patLength.deactivateEvent),
      ) ++ JCB.empty(j.song)

    // override val loadBindings: Seq[Binding[?, ?, ?]] = Vector(
    //   EB(j.step.st.press, "step toggle", () => toggleEvent),
    //   SupBooleanB(j.step.light.isOn, () => isOn),
    // )

    // TODO refactor this already
    // Find the first existing clip on a track
    def findClip(): Option[Clip] =
      secondClip.selectFirst()
      var cnt = 0
      while cnt < 64 && !secondClip.exists().get() do
        cnt += 1
        secondClip.selectNext()
      if secondClip.exists().get() then Some(secondClip) else None

    override def onActivate(): Unit =
      restoreState()
      super.onActivate()

      if selectedClipTrack.position().get() < 0 then selectedClipTrack.selectFirst()

      ext.host.scheduleTask(
        () =>
          if !clip.exists().get() then
            findClip() match
              case None =>
                val t = selectedClipTrack.position().get()
                val c = localState.selectedClips.getOrElse(t, 0)
                selectedClipTrack.createNewLauncherClip(c)
                trackBank.getItemAt(t).clipLauncherSlotBank().select(c)
              case Some(foundClip) =>
                clip.selectClip(foundClip)
                clip.clipLauncherSlot().select()
        ,
        30
      )
    end onActivate

    override def onSeqState(oldSt: SeqState, newSt: SeqState): Unit =
      if modeState._1 == ModeState.Active then
        (newSt.noteVelVisible, velAndNote.isOn) match
          case (true, false) => ext.events.eval("sync track SeqState")(velAndNote.activateEvent*)
          case (false, true) => ext.events.eval("sync track SeqState")(velAndNote.deactivateEvent*)
          case _             => ()

  end stepSequencer
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
