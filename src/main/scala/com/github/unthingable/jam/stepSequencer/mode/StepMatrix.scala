package com.github.unthingable.jam.stepSequencer.mode

import com.github.unthingable.jam.stepSequencer.StepCap
import com.github.unthingable.jam.stepSequencer.TrackedState
import com.github.unthingable.jam.stepSequencer.state.StepState
import com.github.unthingable.jam.stepSequencer.state.PointStep
import com.github.unthingable.jam.stepSequencer.state.Point
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util.EIGHT
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.jam.surface.JamSurface
import com.github.unthingable.framework.mode.ModeLayer

import com.bitwig.extension.controller.api.NoteStep.State as NSState
import com.bitwig.extension.controller.api.NoteStep

import java.time.Instant
import com.github.unthingable.jam.surface.JamColorState

trait StepMatrix(using ext: MonsterJamExt, j: JamSurface) extends StepCap:
  def stepPress(x: Int, y: Int): Unit =
    inline def hasStep: Boolean = stepAt(x, y).state == NSState.NoteOn

    val newState: StepState =
      val step  = stepAt(x, y)
      val pstep = PointStep(Point(x, y), step, Instant.now())
      // simple note preview
      if !ext.transport.isPlaying().get() then
        selectedClipTrack.playNote(y, ts.velocity)
      localState.stepState.get match
        case StepState(Nil, _) =>
          if (step.state == NSState.Empty)
            clip.setStep(ts.channel, x, y, ts.velocity, ts.stepSize)
            StepState(List(PointStep(Point(x, y), stepAt(x, y), Instant.now())), true)
          else StepState(List(pstep), false)
        case st @ StepState(p @ PointStep(Point(x0, y0), _, _) :: Nil, _) if y == y0 && x > x0 && !hasStep =>
          val step0: NoteStep = stepAt(x0, y0)
          val newDuration     = ts.stepSize * (x - x0 + 1)
          if (step0.duration() == newDuration)
            step0.setDuration(ts.stepSize)
          else
            step0.setDuration(newDuration)
          st.copy(noRelease = true)
        case st @ StepState(steps, noRelease) if hasStep =>
          StepState(steps :+ pstep, noRelease)
        case st => st
    localState.stepState.set(newState)
    // Util.println(stepState.toString())

  def stepRelease(X: Int, Y: Int): Unit =
    val newState = localState.stepState.get match
      case StepState(PointStep(Point(X, Y), _, pressed) :: Nil, noRelease) =>
        if (!noRelease && !pressed.isBefore(Instant.now().minusMillis(200)))
          val step = stepAt(X, Y)
          step.state match
            case NSState.NoteOn => clip.clearStep(ts.channel, X, Y)
            case NSState.Empty | NSState.NoteSustain =>
              clip.setStep(ts.channel, X, Y, ts.velocity, ts.stepSize)
        StepState(List.empty, false)
      case st => st.copy(steps = st.steps.filter(_.point != Point(X, Y)))
    localState.stepState.set(newState)
    // Util.println(stepState.toString())

  lazy val stepMatrix = new SimpleModeLayer("stepMatrix") {
    // override def onActivate(): Unit =
    //   super.onActivate()
    // state.stepState.set(StepState(List.empty, false))

    override val modeBindings: Seq[Binding[_, _, _]] =
      (for (col <- EIGHT; row <- EIGHT) yield {
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
                  if (
                    ext.transport.isPlaying
                      .get() && clip.playingStep().get() - ts.stepScrollOffset == xy._1
                  )
                    colorManager.stepPad.playing
                  else
                    colorManager.stepPad.padColor(xy._2, clip.getStep(ts.channel, xy._1, xy._2))
                case _ => JamColorState.empty
          ),
          EB(j.matrix(row)(col).st.press, "", () => xy.foreach(stepPress.tupled)),
          EB(j.matrix(row)(col).st.release, "", () => xy.foreach(stepRelease.tupled))
        )
      }).flatten
  }
