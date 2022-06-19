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
  Noop,
  SupBooleanB,
  SupColorB,
  SupColorStateB
}
import com.github.unthingable.jam.surface.KeyMaster.JC
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.Jam

import scala.collection.mutable
import scala.collection.mutable.ArraySeq
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.BindingDSL

trait StepSequencer extends BindingDSL { this: Jam =>
  enum StepMode:
    case One, Four, Eight
  val stepModeMap = Map(0 -> StepMode.One, 3 -> StepMode.Four, 7 -> StepMode.Eight)

  lazy val stepSequencer = new ModeCycleLayer("STEP") with ListeningLayer {
    val gridHeight               = 8
    val gridWidth                = 64
    val clip: PinnableCursorClip = selectedClipTrack.createLauncherCursorClip(gridWidth, gridHeight)
    val devices: DeviceBank      = selectedClipTrack.createDeviceBank(1)

    // a mirror of the bitwig clip, channel / x / y
    val steps =
      ArraySeq.fill(16)(ArraySeq.fill(gridWidth)(ArraySeq.fill(gridHeight)(null: NoteStep)))

    clip.addNoteStepObserver(ns => steps(ns.channel())(ns.x()).update(ns.y(), ns))

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

    type StepFrac = (Int, Int) | Int
    val stepSizes = Vector[StepFrac](
      1 -> 64,
      1 -> 32,
      1 -> 16,
      1 -> 8,
      1 -> 4,
      1 -> 2,
      1,
      2,
      4,
      8
    )

    def stepSize: Double = stepSizes(stepSizeIdx) match
      case (a, b) => a.toDouble / b.toDouble
      case x: Int => x

    def stepString: String = stepSizes(stepSizeIdx) match
      case (a, b) => s"$a/$b"
      case x: Int => s"$x"

    // state
    var channel                    = 0
    var velocity: Int              = 100
    var stepSizeIdx                = 5
    var drumDevice: Option[Device] = None
    var stepMode: StepMode         = StepMode.One
    var stepScrollOffset           = 0      // can't get it from Clip (for page buttons only)
    var keyScrollOffset            = 12 * 3 // C1
    var keyPageSize                = 1
    var stepPageSize               = 64
    // var newPatLength               = 4
    var stepState = StepState.Init

    clip.setStepSize(stepSize)
    clip.scrollToKey(12 * 3)
    setGrid(StepMode.One)

    // def detectDrum(): Option[Device] = (0 until devices.itemCount().get()).map(devices.getDevice).find(_.hasDrumPads.get())

    // // translate step cache into a bindable
    // def stepView(row: Int, col: Int) = stepMode match {
    //   case StepMode.One =>
    //     // expect 4x8
    //     val stepNum = row * 8 + col
    //     (stepNum, stepNum, keyOffset)
    //   // (stepNum, () => Option(steps(stepNum)(noteOffset)))
    //   case StepMode.Four  => ???
    //   case StepMode.Eight => ???
    // }

    /* Translate between matrix grid (row, col) and clip grid (x, y) */
    def m2clip(row: Int, col: Int): (Int, Int) =
      val offset = row * 8 + col // matrix grid scanned
      (offset % stepPageSize, offset / stepPageSize)

    // inline def clip2m(x: Int, y: Int): (Int, Int) =
    //   val keyPages = 8 / keyPageSize
    //   ((y / keyPageSize) )

    def setGrid(mode: StepMode): Unit =
      mode match
        case StepMode.One =>
          keyPageSize = 1
          stepPageSize = 64
        case StepMode.Four =>
          keyPageSize = 4
          stepPageSize = 16
        case StepMode.Eight =>
          keyPageSize = 8
          stepPageSize = 8
      ext.host.showPopupNotification(s"Step grid: $keyPageSize x $stepPageSize")
      stepMode = mode

    def incStepSize(inc: Int): Unit =
      stepSizeIdx = (stepSizeIdx + inc).min(stepSizes.size - 1).max(0)
      clip.setStepSize(stepSize)
      ext.host.showPopupNotification(s"Step size: $stepString")

    inline def guardX(x: Int) = x // x.max(0).min(gridWidth - 1)

    inline def guardY(y: Int) = y.max(0).min(127)

    // def scrollX(offset: Int) =
    //   stepOffset = guardX(offset)
    //   clip.scrollToStep(stepOffset)

    def scrollY(offset: Int) =
      keyScrollOffset = guardY(offset)
      clip.scrollToKey(keyScrollOffset)

    // def scrollXinc(inc: Int) =
    //   stepOffset = guardX(stepOffset + inc)
    //   clip.scrollToStep(stepOffset)

    def scrollYinc(inc: Int) =
      keyScrollOffset = guardY(keyScrollOffset + inc)
      clip.scrollToKey(keyScrollOffset)

    def setStepPage(page: Int) =
      stepScrollOffset = stepPageSize * page
      clip.scrollToStep(stepScrollOffset)

    case class Point(x: Int, y: Int)
    enum StepState:
      case Init
      case Focus(p: Point, handled: Boolean)

    def stepPress(x: Int, y: Int): Unit =
      val newState = stepState match
        case StepState.Init =>
          val step = clip.getStep(1, x, y)
          if (step.state == State.Empty)
            clip.toggleStep(x, y, 100)
            StepState.Focus(Point(x, y), true)
          else StepState.Focus(Point(x, y), false)
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
      override def onActivate(): Unit =
        super.onActivate()
        stepState = StepState.Init

      override val modeBindings: Seq[Binding[_, _, _]] =
        (for (col <- EIGHT; row <- EIGHT) yield {
          // val (stepNum, x, y) = stepView(row, col)
          def xy = m2clip(row, col)
          // def cachedClip = steps(channel)(xy._1)(xy._2)
          Vector(
            SupColorStateB(
              j.matrix(row)(col).light,
              () =>
                // chasing light
                if (
                  ext.transport.isPlaying.get() && clip.playingStep().get() == xy._1
                ) // not right yet
                  JamColorState(JamColorBase.WHITE, 1)
                else {
                  clip.getStep(0, xy._1, xy._2).state() match {
                    case State.NoteOn      => JamColorState(clip.color().get(), 1)
                    case State.NoteSustain => JamColorState(JamColorBase.WHITE, 0)
                    case State.Empty       => JamColorState.empty
                  }
                }
            ),
            EB(j.matrix(row)(col).st.press, "", () => stepPress.tupled(xy)),
            EB(j.matrix(row)(col).st.release, "", () => stepRelease.tupled(xy))
          )
        }).flatten ++
          j.sceneButtons.zipWithIndex.flatMap { (btn, i) =>
            def hasContent = clip.getLoopLength().get() > i * stepPageSize

            Vector(
              EB(btn.st.press, "", () => if (hasContent) setStepPage(i)),
              SupColorB(
                btn.light,
                () =>
                  if (hasContent)
                    // if (i == stepOffset / 32) Color.whiteColor() else clip.color().get()
                    if (i == stepScrollOffset / stepPageSize) Color.whiteColor()
                    else clip.color().get()
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
            val newPatLength = idx + 1
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
              SupColorB(
                btn.light,
                () => if (stepMode == sm) Color.whiteColor() else clip.color().get
              )
            )
      } ++ Vector(
        HB(j.encoder.touch.pressedAction, "", () => incStepSize(0)),
        HB(j.encoder.turn, "", stepTarget(() => incStepSize(1), () => incStepSize(-1)))
      )
    )

    override val subModes: Seq[ModeLayer] = Vector(
      stepsLayer,
      patLength,
      gridSelect,
    )

    override val modeBindings: Seq[Binding[_, _, _]] =
      Vector(
        EB(j.ShiftSolo.press, "shift-solo pressed", () => cycle(0, 1)),
        EB(j.ShiftSolo.releaseAll, "shift-solo released", () => select(0)),
        // TODO next: multiline
        EB(j.dpad.up.st.press, "scroll page up", () => scrollYinc(keyPageSize)),
        EB(j.dpad.down.st.press, "scroll page down", () => scrollYinc(-1 * keyPageSize)),
        EB(j.dpad.left.st.press, "", Noop),
        EB(j.dpad.right.st.press, "", Noop),
        SupBooleanB(j.dpad.up.light.isOn(), clip.canScrollKeysUp()),
        SupBooleanB(j.dpad.down.light.isOn(), clip.canScrollKeysDown()),
        SupBooleanB(j.dpad.left.light.isOn(), () => false),
        SupBooleanB(j.dpad.right.light.isOn(), () => false),
      ) ++ gridSelect.loadBindings

    override val loadBindings: Seq[Binding[_, _, _]] = Vector(
      EB(j.step.st.press, "step toggle", () => toggleEvent.value),
      SupBooleanB(j.step.light.isOn, () => isOn),
    )
  }
}
