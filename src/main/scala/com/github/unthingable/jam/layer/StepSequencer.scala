package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.NoteStep.State
import com.bitwig.extension.controller.api.{Device, DeviceBank, NoteStep, PinnableCursorClip}
import com.github.unthingable.Util
import com.github.unthingable.framework.mode.{
  ListeningLayer,
  ModeCycleLayer,
  ModeLayer,
  MultiModeLayer,
  SimpleModeLayer
}
import com.github.unthingable.framework.binding.{
  Binding,
  EB,
  JCB,
  SupBooleanB,
  SupColorB,
  SupColorStateB,
  Noop
}
import com.github.unthingable.jam.surface.KeyMaster.JC
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.Jam

import scala.collection.mutable
import scala.collection.mutable.ArraySeq

trait StepSequencer { this: Jam =>
  enum StepMode:
    case One, Four, Eight

  lazy val stepSequencer = new ModeCycleLayer("STEP") with ListeningLayer {
    val rows                     = 8
    val cols                     = 128
    val clip: PinnableCursorClip = selectedClipTrack.createLauncherCursorClip(cols, rows)
    val devices: DeviceBank      = selectedClipTrack.createDeviceBank(1)

    // clip.addNoteStepObserver(ns => steps(ns.x()).update(ns.y(), ns))

    devices.itemCount().addValueObserver(v => Util.println(v.toString))
    clip.getPlayStop.addValueObserver(v => Util.println(s"beats $v"))
    // clip.playingStep().addValueObserver(v => Util.println(s"playing step $v"))

    // follow track selection
    ext.cursorTrack
      .position()
      .addValueObserver(v => selectedClipTrack.selectChannel(ext.cursorTrack))

    Vector(
      clip.getPlayStart,
      clip.getPlayStop,
      clip.getAccent,
      clip.getShuffle,
      clip.getLoopStart,
      clip.getLoopLength,
      clip.playingStep(),
      clip.color(),
      clip.canScrollKeysDown(),
      clip.canScrollKeysUp(),
      selectedClipTrack.color,
      ext.transport.isPlaying,
      devices.itemCount(), // hopefully this gets updated
    ).foreach(_.markInterested())

    // state
    var velocity: Int              = 100
    var stepSize                   = 0.25
    var drumDevice: Option[Device] = None
    var stepMode: StepMode         = StepMode.One
    var noteOffset                 = 0
    var stepOffset                 = 0
    var newPatLength               = 4

    clip.setStepSize(stepSize)
    clip.scrollToKey(12 * 3)

    // def detectDrum(): Option[Device] = (0 until devices.itemCount().get()).map(devices.getDevice).find(_.hasDrumPads.get())

    // translate step cache into a bindable
    def stepView(row: Int, col: Int) = stepMode match {
      case StepMode.One =>
        // expect 4x8
        val stepNum = row * 8 + col
        (stepNum, stepNum, noteOffset)
      // (stepNum, () => Option(steps(stepNum)(noteOffset)))
      case StepMode.Four  => ???
      case StepMode.Eight => ???
    }

    def scrollX(offset: Int) = {
      clip.scrollToStep(offset)
      stepOffset = offset
    }

    case class Point(x: Int, y: Int)
    enum StepState:
      case Init
      case Focus(p: Point, handled: Boolean)

    var stepState = StepState.Init

    def stepPress(x: Int, y: Int): Unit =
      val newState = stepState match
        case StepState.Init =>
          val step = clip.getStep(1, x, y)
          if (step.state == State.Empty)
            clip.toggleStep(x, y, 100)
            StepState.Focus(Point(x, y), true)
          else
            StepState.Focus(Point(x, y), false)
        case st @ StepState.Focus(p @ Point(x0, y0), _) if y == y0 && x > x0 =>
          val step: NoteStep = clip.getStep(0, x0, y0)
          val newDuration    = stepSize * (x - x0 + 1)
          if (step.duration() == newDuration)
            step.setDuration(stepSize)
          else
            step.setDuration(newDuration)
          st.copy(handled = true)
        case st => st
      stepState = newState
      Util.println(stepState.toString())

    def stepRelease(X: Int, Y: Int): Unit =
      val newState = stepState match
        case StepState.Focus(Point(X, Y), handled) =>
          if (!handled)
            clip.toggleStep(X, Y, 100)
          StepState.Init
        case st => st
      stepState = newState
      Util.println(stepState.toString())

    lazy val stepsLayer = new SimpleModeLayer("steps") {
      inline val numSteps = 32

      override def onActivate(): Unit = 
        super.onActivate()
        stepState = StepState.Init

      override val modeBindings: Seq[Binding[_, _, _]] =
        (for (col <- EIGHT; row <- EIGHT) yield {
          val (stepNum, x, y) = stepView(row, col)
          def step = clip.getStep(0, x, y)
          Vector(
            SupColorStateB(
              j.matrix(row)(col).light,
              () =>
                // chasing light
                if (
                  ext.transport.isPlaying.get() && clip.playingStep().get() == x + stepOffset
                ) // not right yet
                  JamColorState(JamColorBase.WHITE, 1)
                else {
                  step.state() match {
                    case State.NoteOn      => JamColorState(clip.color().get(), 1)
                    case State.NoteSustain => JamColorState(JamColorBase.WHITE, 0)
                    case State.Empty       => JamColorState.empty
                  }
                }
            ),
            EB(j.matrix(row)(col).st.press, "", () => stepPress(x, y)),
            EB(j.matrix(row)(col).st.release, "", () => stepRelease(x, y))
          )
        }).flatten ++
          EIGHT.flatMap { i =>
            val btn        = j.sceneButtons(i)
            def hasContent = clip.getLoopLength().get() > i * numSteps * stepSize

            Vector(
              EB(btn.st.press, "", () => if (hasContent) scrollX(i * numSteps)),
              SupColorB(
                btn.light,
                () =>
                  if (hasContent)
                    if (i == stepOffset / 32) Color.whiteColor() else clip.color().get()
                  else Color.blackColor()
              ),
            )
          }
    }

    lazy val patLength = new SimpleModeLayer("patLength") {
      override def modeBindings: Seq[Binding[_, _, _]] = (0 until 64).flatMap { idx =>
        JCB(
          id,
          j.matrix(idx / 8)(idx % 8),
          () => {
            Util.println(s"set playStop $idx")
            newPatLength = idx + 1
            clip.getLoopLength.set(newPatLength.toDouble)
            clip.getPlayStop.set(newPatLength.toDouble) // doesn't follow the first length change
          },
          () => (),
          () =>
            if (clip.getPlayStop.get() > idx)
              JamColorState(clip.color().get(), 2)
            else
              JamColorState.empty
        )
      } ++ Vector(SupBooleanB(j.solo.light.isOn, () => true))
    }

    override val subModes: Seq[ModeLayer] = Vector(
      stepsLayer,
      patLength
    )

    override val modeBindings: Seq[Binding[_, _, _]] =
      Vector(
        EB(j.ShiftSolo.press, "shift-solo pressed", () => cycle()),
        EB(j.ShiftSolo.releaseAll, "shift-solo released", () => select(0)),
        EB(j.dpad.up.st.press, "scroll page up", () => clip.scrollKeysPageUp()),
        EB(j.dpad.down.st.press, "scroll page down", () => clip.scrollKeysPageDown()),
        EB(j.dpad.left.st.press, "", Noop),
        EB(j.dpad.right.st.press, "", Noop),
        SupBooleanB(j.dpad.up.light.isOn(), clip.canScrollKeysUp()),
        SupBooleanB(j.dpad.down.light.isOn(), clip.canScrollKeysDown()),
        SupBooleanB(j.dpad.left.light.isOn(), () => false),
        SupBooleanB(j.dpad.right.light.isOn(), () => false),
      )

    override val loadBindings: Seq[Binding[_, _, _]] = Vector(
      EB(j.step.st.press, "step toggle", () => toggleEvent.value),
      SupBooleanB(j.step.light.isOn, () => isOn),
    )
  }
}
