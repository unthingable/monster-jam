package com.github.unthingable.jam.stepSequencer.mode

import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State as NSState
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util.EIGHT
import com.github.unthingable.Util.trace
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.stepSequencer.StepCap
import com.github.unthingable.jam.stepSequencer.TrackedState
import com.github.unthingable.jam.stepSequencer.state.Point
import com.github.unthingable.jam.stepSequencer.state.PointStep
import com.github.unthingable.jam.stepSequencer.state.StepState
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamSurface

import java.time.Instant

trait StepMatrix(using ext: MonsterJamExt, j: JamSurface) extends StepCap:
  def stepPress(x: Int, y: Int, row: Int, col: Int): Unit =
    inline def hasStep: Boolean = stepAt(x, y).state == NSState.NoteOn

    val newState: StepState =
      def step  = stepAt(x, y)
      val pstep = PointStep(Point(row, col), step, Instant.now())

      // simple note preview
      def playMe(): Unit =
        if !ext.transport.isPlaying().get() && ext.preferences.stepNotePreview.get() then
          selectedClipTrack.playNote(y, ts.velocity)

      localState.stepState.get match
        // no steps held
        case StepState(Nil, _) =>
          playMe()
          if step.state == NSState.Empty then
            clip.setStep(ts.channel, x, y, ts.velocity, ts.stepSize)
            StepState(List(pstep), true)
          else StepState(List(pstep), false)
        // one step already held and this one is blank
        case st @ StepState(p @ PointStep(_, s0, _) :: Nil, _) if y == s0.y && x > s0.x && !hasStep =>
          // val step0: NoteStep = stepAt(s0.x, s0.y)
          val newDuration = ts.stepSize * (x - s0.x + 1)
          if s0.duration() == newDuration then s0.setDuration(ts.stepSize)
          else s0.setDuration(newDuration)
          st.copy(noRelease = true)
        // pressing more steps
        case st @ StepState(steps, noRelease) if hasStep =>
          playMe()
          StepState(steps :+ pstep, noRelease)
        case st => st
      end match
    end newState
    localState.stepState.set(newState)
    // Util.println(stepState.toString())
  end stepPress

  def stepRelease(X: Int, Y: Int, Row: Int, Col: Int): Unit =
    val newState = localState.stepState.get match
      case StepState(PointStep(Point(Row, Col), step, pressed) :: Nil, noRelease) =>
        if !noRelease && !pressed.isBefore(Instant.now().minusMillis(200)) then
          step.state match
            case NSState.NoteOn => clip.clearStep(ts.channel, step.x, step.y)
            case NSState.Empty | NSState.NoteSustain =>
              clip.setStep(ts.channel, step.x, step.y, ts.velocity, ts.stepSize)
        StepState(List.empty, false)
      case st => st.copy(steps = st.steps.filter(_.point != Point(Row, Col)))
    localState.stepState.set(newState)
    // Util.println(stepState.toString())

  lazy val stepMatrix = new SimpleModeLayer("stepMatrix"):
    override val modeBindings: Seq[Binding[?, ?, ?]] =
      (for (col <- EIGHT; row <- EIGHT) yield
        // val (stepNum, x, y) = stepView(row, col)
        def xy: Option[(Int, Int)] = m2clip(row, col)
        // def cachedClip = steps(channel)(xy._1)(xy._2)
        Vector(
          SupColorStateB(
            j.matrix(row)(col).light,
            () =>
              // chasing light
              xy match
                case Some(xy) =>
                  if ext.transport.isPlaying
                      .get() && clip.playingStep().get() - ts.stepScrollOffset == xy._1
                  then colorManager.stepPad.playing
                  else colorManager.stepPad.padColor(xy._2, clip.getStep(ts.channel, xy._1, xy._2))
                case _ =>
                  JamColorState.empty
                // FiXME
                // rowSelected
                //   .filter(_ == xy.get._2)
                //   .map(_ => colorManager.stepPad.rowSelected)
                //   .getOrElse(JamColorState.empty)
          ),
          EB(j.matrix(row)(col).st.press, "", () => xy.foreach((x, y) => stepPress(x, y, row, col))),
          EB(j.matrix(row)(col).st.release, "", () => xy.foreach((x, y) => stepRelease(x, y, row, col)))
        )
      ).flatten
end StepMatrix
