package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.NoteStep.State
import com.bitwig.extension.controller.api.{Device, DeviceBank, NoteStep, PinnableCursorClip}
import com.github.unthingable.Util
import com.github.unthingable.jam.binding.{Binding, HB, SupBooleanB, SupColorB, SupColorStateB}
import com.github.unthingable.jam.layer.StepMode.{Eight, Four, One}
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.{Jam, ListeningLayer, SimpleModeLayer}

import scala.collection.mutable
import scala.collection.mutable.ArraySeq

object StepMode extends Enumeration {
  val One, Four, Eight = Value
}

trait StepSequencer { this: Jam =>

  lazy val stepSequencer = new SimpleModeLayer("STEP") with ListeningLayer {
    val rows = 8
    val cols = 128
    val clip   : PinnableCursorClip = selectedClipTrack.createLauncherCursorClip(cols, rows)
    val devices: DeviceBank         = selectedClipTrack.createDeviceBank(1)

    clip.addNoteStepObserver(ns => steps(ns.x()).update(ns.y(), ns))

    devices.itemCount().addValueObserver(v => Util.println(v.toString))
    clip.getPlayStop.addValueObserver(v => Util.println(s"beats $v"))

    // follow track selection
    ext.cursorTrack.position().addValueObserver(v => if (isOn) selectedClipTrack.selectChannel(ext.cursorTrack))

    Vector(
      clip.getPlayStart,
      clip.getPlayStop,
      clip.getAccent,
      clip.getShuffle,
      clip.getLoopStart,
      clip.getLoopLength,
      clip.playingStep(),
      selectedClipTrack.color,
      ext.transport.isPlaying,
      devices.itemCount(), // hopefully this gets updated
    ).foreach(_.markInterested())

    // state
    var velocity: Int = 100
    var stepSize = 0.25
    var patLength = 4
    var drumDevice: Option[Device] = None
    var stepMode: StepMode.Value = StepMode.One
    var noteOffset = 0

    // a mirror of the bitwig clip
    val steps: mutable.ArraySeq[mutable.ArraySeq[NoteStep]] = ArraySeq.fill(cols)(ArraySeq.fill(rows)(null))

    clip.setStepSize(stepSize)

    //def detectDrum(): Option[Device] = (0 until devices.itemCount().get()).map(devices.getDevice).find(_.hasDrumPads.get())

    // translate step cache into a bindable
    def stepView(row: Int, col: Int) = stepMode match {
      case One   =>
        // expect 4x8
        val stepNum = row * 8 + col
        (stepNum, stepNum, noteOffset)
        //(stepNum, () => Option(steps(stepNum)(noteOffset)))
      case Four  => ???
      case Eight => ???
    }

    override val modeBindings: Seq[Binding[_, _, _]] =
      (for (col <- (0 until 8); row <- (0 until 4)) yield {
        val (stepNum, x, y) = stepView(row, col)
        Vector(
          SupColorStateB(j.matrix(row)(col).light, () => {
            // chasing light
            if (ext.transport.isPlaying.get() && clip.playingStep().get() == col) // not right yet
              JamColorState(JAMColorBase.WHITE, 1)
            else {
              Option(steps(x)(y)).map(_.state() match {
                case State.NoteOn      => JamColorState(JAMColorBase.RED, 1)
                case State.NoteSustain => JamColorState(JAMColorBase.WHITE, 0)
                case State.Empty       => JamColorState.empty
              }).getOrElse(JamColorState.empty)
            }
          }),
          HB(j.matrix(row)(col).pressedAction, "", () => clip.toggleStep(x, y, 100)))
      }).flatten ++
      EIGHT.flatMap { i =>
        val btn = j.sceneButtons(i)
        def hasContent = clip.getPlayStop.get() > i * 32 * stepSize

        Vector(
          HB(btn.pressedAction, "", () => if (hasContent) clip.scrollToStep(i * 32)),
          SupColorB(btn.light, () => if (hasContent) selectedClipTrack.color().get() else Color.blackColor())
        )
      }

    override val loadBindings: Seq[Binding[_, _, _]] = Vector(
      HB(j.step.pressedAction, "step toggle", () => toggleAction.invoke()),
      SupBooleanB(j.step.light.isOn, () => isOn),
    )
  }
}
