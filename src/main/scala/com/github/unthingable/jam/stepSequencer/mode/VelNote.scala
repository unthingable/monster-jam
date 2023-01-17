package com.github.unthingable.jam.stepSequencer.mode

import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.surface.JamSurface
import com.github.unthingable.jam.stepSequencer.StepCap
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.jam.stepSequencer.state.*
import com.github.unthingable.framework.binding.SupColorB
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.framework.binding.Binding

import com.bitwig.extension.api.Color
import com.github.unthingable.Util
import Util.trace
import com.bitwig.extension.controller.api.NoteStep

import scala.collection.mutable
import com.github.unthingable.framework.mode.ModeLayer

trait VelNote(using ext: MonsterJamExt, j: JamSurface) extends StepCap:
  def stepMain: ModeLayer

  lazy val velAndNote =
    new ModeButtonLayer("velAndNote", j.notes, gateMode = GateMode.SmartRelease) {
      selectedClipTrack.playingNotes().markInterested()
      override def onActivate(): Unit =
        super.onActivate()
        if isCurrent(ts) then setState(ts.copy(noteVelVisible = true))

      override def onDeactivate(): Unit =
        super.onDeactivate()
        // liveness hack: if stepMain is off, sequencer is being deactivated so preserve velNote state
        if stepMain.isOn && isCurrent(ts) then setState(ts.copy(noteVelVisible = false))
        playingNotes.foreach((_, n) => selectedClipTrack.stopNote(n.value, ts.velocity))
        playingNotes.clear()

      inline def velNote(vel: Int) = s"Step: velocity $vel"

      /** Velocity of either a last pressed step or default */
      def getVelocity: Int =
        localState.stepState.get.steps.lastOption
          .filter(_.step.state() != NoteStep.State.Empty)
          .map(s => (s.step.velocity() * 128).toInt)
          .getOrElse(ts.velocity)

      /** Set default velocity */
      def setVelocity(vel: Int) =
        ext.host.showPopupNotification(velNote(vel)) // TODO consolidate
        val steps = localState.stepState.get.steps
        if (steps.nonEmpty) steps.foreach(_.step.setVelocity(vel / 128.0))
        else
          setState(ts.copy(velocity = vel))
          selectedClipTrack.playNote(ts.fromScale(ts.keyScrollOffsetGuarded).value, vel)

      val playingNotes = mutable.Map.empty[Int, RealNote]

      def notePress(idx: Int, note: ScaledNote): Unit =
        if j.select.st.isPressed then scrollYTo(note)
        else scrollYTo((note.asInstanceOf[Int] / ts.keyPageSize * ts.keyPageSize).asInstanceOf[ScaledNote])

        val realNote = ts.fromScale(note)
        playingNotes.get(idx).foreach(n => selectedClipTrack.stopNote(n.value, ts.velocity))
        playingNotes.update(idx, realNote)
        selectedClipTrack.startNote(realNote.value, ts.velocity)

      def noteRelease(idx: Int): Unit =
        playingNotes
          .get(idx)
          .foreach(note =>
            selectedClipTrack.stopNote(note.value, ts.velocity)
            playingNotes.drop(idx)
            Util.println(SeqState.toNoteName(note))
          )

      val velBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn             = j.matrix(7 - row)(col)
        inline val velScale = 8
        val idx             = row * 4 + col
        val vel             = idx * velScale + (velScale - 1)
        Vector(
          SupColorStateB(
            btn.light,
            () => JamColorState(if (getVelocity / velScale == idx) Color.whiteColor() else clipColor, 1)
          ),
          EB(btn.st.press, velNote(vel), () => setVelocity(vel))
        )

      def pageOffset = notePageOffset(noteToPageIdx(ts.keyScrollOffsetGuarded))

      val noteBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn = j.matrix(row + 4)(col + 4)

        val idx           = (3 - row) * 4 + col
        def scaledNoteIdx = pageOffset.asInstanceOf[Int] + idx
        def scaledNote    = scaledNoteIdx.asInstanceOf[ScaledNote]
        def realNote      = ts.fromScale(scaledNote)

        Vector(
          SupColorStateB(
            btn.light,
            () =>
              if (scaledNote == ts.keyScrollOffsetGuarded)
                JamColorState(Color.whiteColor(), 3)
              else if (ts.isNoteVisible(scaledNote))
                JamColorState(Color.whiteColor(), 1)
              else
                JamColorState(
                  colorManager.stepPad.noteColor(scaledNoteIdx),
                  if (selectedClipTrack.playingNotes().isNotePlaying(realNote.value)) 3 else 0
                )
          ),
          EB(btn.st.press, "set note", () => notePress(idx, scaledNote)),
          EB(btn.st.release, "release note", () => noteRelease(idx)),
        )
      override val modeBindings = velBindings.flatten ++ noteBindings.flatten
    }

  val pageOffsets = Vector(0, 4, 20, 36, 52, 68, 84, 100, 116, 132).map(_.asInstanceOf[ScaledNote]) // for chromatic

  /** First note on page corresponding to number */
  def notePageOffset(idx: Int): ScaledNote =
    if ts.scale.isChromatic then pageOffsets(idx) else (idx * 16).asInstanceOf[ScaledNote]

  /** Index of page containing note */
  def noteToPageIdx(note: ScaledNote): Int =
    if ts.scale.isChromatic then
      val idx = pageOffsets.lastIndexWhere(_.asInstanceOf[Int] <= note.asInstanceOf[Int])
      if idx > -1 then idx else pageOffsets.size - 1
    else note.asInstanceOf[Int] / 16

  lazy val notePages =
    new ModeButtonLayer("notePages", j.notes, gateMode = GateMode.Gate, silent = true) {
      inline def keyOffset: ScaledNote = ts.keyScrollOffsetGuarded
      inline def hasContent(idx: Int) =
        val note       = keyOffset.asInstanceOf[Int]
        val pageOffset = notePageOffset(idx).asInstanceOf[Int]
        // detect overlap
        (note + 16 > pageOffset && note + 16 < pageOffset + 16) || (note < pageOffset + 16 && note > pageOffset)

      override def modeBindings: Seq[Binding[?, ?, ?]] =
        j.sceneButtons.zipWithIndex.flatMap((btn, idx) =>
          Vector(
            EB(
              btn.st.press,
              "",
              () => scrollYTo(notePageOffset(idx).asInstanceOf[ScaledNote])
            ),
            SupColorStateB(
              btn.light,
              () =>
                (notePageOffset(idx) == keyOffset, hasContent(idx)) match
                  case (true, _)                                                => JamColorState(JamColorBase.WHITE, 2)
                  case (_, true)                                                => JamColorState(JamColorBase.WHITE, 0)
                  case _ if ts.scale.isChromatic || idx * 16 <= ts.scale.length => JamColorState(JamColorBase.CYAN, 2)
                  case _                                                        => JamColorState.empty
            )
          )
        )
    }
