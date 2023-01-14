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
import com.bitwig.extension.controller.api.NoteStep

trait VelNote(using ext: MonsterJamExt, j: JamSurface) extends StepCap:
  lazy val velAndNote =
    new ModeButtonLayer("velAndNote", j.notes, gateMode = GateMode.AutoInverse) {
      selectedClipTrack.playingNotes().markInterested()
      override def onActivate(): Unit =
        super.onActivate()
        setState(ts.copy(noteVelVisible = true))
        // state.stepViewPort.set(ViewPort(0, 0, 4, 8))

      override def onDeactivate(): Unit =
        super.onDeactivate()
        setState(ts.copy(noteVelVisible = false))
        // state.stepViewPort.set(ViewPort(0, 0, 8, 8))

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
          selectedClipTrack.playNote(ts.fromScale(ts.keyScaledOffset).value, vel)

      def notePress(note: ScaledNote): Unit =
        scrollYTo(note)
        selectedClipTrack.startNote(ts.fromScale(note).value, ts.velocity)

      def noteRelease(note: RealNote): Unit =
        selectedClipTrack.stopNote(note.value, ts.velocity)
        Util.println(SeqState.toNoteName(note))

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

      def pageOffset = ts.keyScaledOffset.asInstanceOf[Int] / 16

      val noteBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn = j.matrix(row + 4)(col + 4)

        def scaledNoteIdx = pageOffset * 16 + (3 - row) * 4 + col
        def scaledNote    = scaledNoteIdx.asInstanceOf[ScaledNote]
        def realNote             = ts.fromScale(scaledNote)

        Vector(
          SupColorStateB(
            btn.light,
            () =>
              if (realNote == ts.keyScrollOffsetGuarded)
                JamColorState(Color.whiteColor(), 3)
              else if (ts.isNoteVisible(scaledNote))
                JamColorState(Color.whiteColor(), 1)
              else
                JamColorState(
                  colorManager.stepPad.noteColor(scaledNoteIdx),
                  if (selectedClipTrack.playingNotes().isNotePlaying(realNote.value)) 3 else 0
                )
          ),
          EB(btn.st.press, "set note", () => notePress(scaledNote)),
          EB(btn.st.release, "release note", () => noteRelease(realNote)),
        )
      override val modeBindings = velBindings.flatten ++ noteBindings.flatten
    }

  val pageOffsets = Vector(0, 4, 20, 36, 52, 68, 84, 100, 116, 132).map(_.asInstanceOf[ScaledNote]) // for chromatic
  def notePageOffset(idx: Int): ScaledNote = if ts.scale.isChromatic then pageOffsets(idx) else (idx * 16).asInstanceOf[ScaledNote]
  
  lazy val notePages =
    new ModeButtonLayer("notePages", j.notes, gateMode = GateMode.Gate, silent = true) {
      inline def keyOffset: ScaledNote   = ts.keyScaledOffset
      inline def hasContent(idx: Int) = 
         val note = keyOffset.asInstanceOf[Int]
         val pageOffset = notePageOffset(idx).asInstanceOf[Int]
         // detect overlap
         (note + 16 > pageOffset && note + 16 < pageOffset + 16) || (note < pageOffset + 16 && note > pageOffset)

      override def modeBindings: Seq[Binding[?, ?, ?]] =
        j.sceneButtons.zipWithIndex.flatMap((btn, idx) =>
          Vector(
            EB(
              btn.st.press,
              "",
              () => scrollYTo((notePageOffset(idx)).asInstanceOf[ScaledNote])
            ),
            SupColorStateB(
              btn.light,
              () =>
                (notePageOffset(idx) == keyOffset, hasContent(idx)) match
                  case (true, _) => JamColorState(JamColorBase.WHITE, 2)
                  case (_, true) => JamColorState(JamColorBase.WHITE, 0)
                  case _         => JamColorState(JamColorBase.CYAN, 2)
            )
          )
        )
    }
