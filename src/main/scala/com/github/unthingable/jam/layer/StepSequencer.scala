package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.NoteStep.State as NSState
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
  // Noop,
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
import java.time.Instant
  
trait StepSequencer extends BindingDSL { this: Jam =>
  enum StepMode(val keyRows: Int):
    case One extends StepMode(1)
    // case OneFull extends StepMode(1)
    case Four extends StepMode(4)
    case Eight extends StepMode(8)

  val stepModeMap = Map(
    0 -> StepMode.One, 
    // 1 -> StepMode.OneFull,
    3 -> StepMode.Four, 
    7 -> StepMode.Eight
  )

  case class ViewPort(row1: Int, col1: Int, row2: Int, col2: Int):
    lazy val height = row2 - row1
    lazy val width = col2 - col1
    lazy val size = height * width

  case class Point(x: Int, y: Int)

  enum StepState:
    case Init
    case Focus(p: Point, handled: Boolean, ts: Instant)

  inline def Noop = () => Vector.empty

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

    inline def stepSize: Double = stepSizes(state.stepSizeIdx) match
      case (a, b) => a.toDouble / b.toDouble
      case x: Int => x

    inline def stepString: String = stepSizes(state.stepSizeIdx) match
      case (a, b) => s"$a/$b"
      case x: Int => s"$x"

    object state:
      var channel                    = 0
      var velocity: Int              = 100
      var stepSizeIdx                = 5
      var drumDevice: Option[Device] = None
      var stepMode: StepMode         = StepMode.One
      var stepScrollOffset           = 0      // can't get it from Clip (for page buttons only)
      var keyScrollOffset            = 12 * 3 // C1
      // var keyPageSize                = 1
      // var stepPageSize               = 64
      // var newPatLength               = 4
      var stepState = StepState.Init
      var stepViewPort = ViewPort(0,0,8,8) // row/col

    clip.setStepSize(stepSize)
    clip.scrollToKey(12 * 3)
    setGrid(StepMode.One)

    // def detectDrum(): Option[Device] = (0 until devices.itemCount().get()).map(devices.getDevice).find(_.hasDrumPads.get())

    /* Translate between matrix grid (row, col) and clip grid (x, y) */
    def m2clip(row: Int, col: Int): (Int, Int) =
      // no need to account for view port as long as starts at 0,0
      val offset = row * 8 + col // matrix grid scanned
      (offset % stepPageSize, 7 - (offset / stepPageSize))

    def setGrid(mode: StepMode): Unit =
      state.stepMode = mode
      ext.host.showPopupNotification(s"Step grid: ${keyPageSize} x ${stepPageSize}")

    inline def keyPageSize = (state.stepMode.keyRows / (8 / state.stepViewPort.height)).max(1)
    
    inline def stepPageSize = state.stepViewPort.size / keyPageSize

    def incStepSize(inc: Int): Unit =
      state.stepSizeIdx = (state.stepSizeIdx + inc).min(stepSizes.size - 1).max(0)
      clip.setStepSize(stepSize)
      ext.host.showPopupNotification(s"Step size: $stepString")

    inline def guardY(y: Int) = y.max(0).min(127)

    def scrollY(offset: Int) =
      state.keyScrollOffset = guardY(offset)
      clip.scrollToKey(state.keyScrollOffset)

    def scrollYinc(inc: Int) =
      state.keyScrollOffset = guardY(state.keyScrollOffset + inc)
      clip.scrollToKey(state.keyScrollOffset)

    def setStepPage(page: Int) =
      state.stepScrollOffset = stepPageSize * page
      clip.scrollToStep(state.stepScrollOffset)

    def stepPress(x: Int, y: Int): Unit =
      val newState = state.stepState match
        case StepState.Init =>
          val step = clip.getStep(state.channel, x, y)
          if (step.state == NSState.Empty)
            clip.setStep(state.channel, x, y, 100, stepSize)
            StepState.Focus(Point(x, y), true, Instant.now())
          else StepState.Focus(Point(x, y), false, Instant.now())
        case st @ StepState.Focus(p @ Point(x0, y0), _, _) if y == y0 && x > x0 =>
          val step: NoteStep = clip.getStep(state.channel, x0, y0)
          val newDuration    = stepSize * (x - x0 + 1)
          if (step.duration() == newDuration)
            step.setDuration(stepSize)
          else
            step.setDuration(newDuration)
          st.copy(handled = true)
        case st => st
      state.stepState = newState
      // Util.println(stepState.toString())

    def stepRelease(X: Int, Y: Int): Unit =
      val newState = state.stepState match
        case StepState.Focus(Point(X, Y), handled, ts) =>
          if (!handled)
            val step = clip.getStep(state.channel, X, Y)
            step.state match
              case NSState.NoteOn => clip.clearStep(state.channel, X, Y)
              case NSState.Empty | NSState.NoteSustain => clip.setStep(state.channel, X, Y, 100, stepSize)
          StepState.Init
        case st => st
      state.stepState = newState
      // Util.println(stepState.toString())

    lazy val stepMatrix = new SimpleModeLayer("stepMatrix") {
      override def onActivate(): Unit =
        super.onActivate()
        state.stepState = StepState.Init

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
                  ext.transport.isPlaying.get() && clip.playingStep().get() - state.stepScrollOffset == xy._1
                ) // not right yet
                  JamColorState(JamColorBase.WHITE, 1)
                else {
                  clip.getStep(0, xy._1, xy._2).state() match {
                    case NSState.NoteOn      => JamColorState(clip.color().get(), 1)
                    case NSState.NoteSustain => JamColorState(JamColorBase.WHITE, 0)
                    case NSState.Empty       => JamColorState.empty
                  }
                }
            ),
            EB(j.matrix(row)(col).st.press, "", () => stepPress.tupled(xy)),
            EB(j.matrix(row)(col).st.release, "", () => stepRelease.tupled(xy))
          )
        }).flatten
    }

    lazy val stepPages = new SimpleModeLayer("stepPages") {
      override val modeBindings =
        j.sceneButtons.zipWithIndex.flatMap { (btn, i) =>
          def hasContent = clip.getLoopLength().get() > i * stepSize * stepPageSize
          Vector(
            EB(btn.st.press, "", () => if (hasContent) setStepPage(i)),
            SupColorStateB(
              btn.light,
              () =>
                if (hasContent)
                  // if (i == stepOffset / 32) Color.whiteColor() else clip.color().get()
                  if (i == state.stepScrollOffset / stepPageSize) JamColorState(JamColorBase.WHITE, 3)
                  else if (clip.playingStep().get() / stepPageSize == i)
                    JamColorState(JamColorBase.WHITE, 0)
                  else JamColorState(clip.color().get(), 1)
                else JamColorState.empty
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
              SupColorStateB(
                btn.light,
                () => if (state.stepMode == sm) JamColorState(Color.whiteColor(), 3) else JamColorState(clip.color().get, 2)
              )
            )
      } ++ Vector(
        HB(j.encoder.touch.pressedAction, "", () => incStepSize(0)),
        HB(j.encoder.turn, "", stepTarget(() => incStepSize(1), () => incStepSize(-1)))
      ),
    )

    def setChannel(ch: Int) = 
      ext.host.showPopupNotification(s"Step sequencer: MIDI channel ${ch+1}")
      state.channel = ch

    lazy val chanSelect = ModeButtonLayer(
      "chanSelect",
      j.perform,
      (for (row <- 0 until 4; col <- 0 until 4) yield
        val btn = j.matrix(row + 4)(col + 4)
        val idx = row * 4 + col
        Vector(
          SupColorB(btn.light, () => if (idx == state.channel) Color.whiteColor else clip.color().get),
          EB(btn.st.press, s"select channel $idx", () => setChannel(idx))
        )
      ).flatten,
      GateMode.Gate
    )

    lazy val velAndNote = new ModeButtonLayer("velAndNote", j.notes) {
      override def onActivate(): Unit = 
        super.onActivate()
        state.stepViewPort = ViewPort(0,0,4,8)

      override def onDeactivate(): Unit = 
        super.onDeactivate()
        state.stepViewPort = ViewPort(0,0,8,8)

      inline def velNote(vel: Int) = s"Step: velocity $vel"

      def setVelocity(vel: Int) =
        ext.host.showPopupNotification(velNote(vel))
        state.velocity = vel

      val velBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn = j.matrix(row + 4)(col)
        inline val velScale = 8
        val idx = row * 4 + col
        val vel = idx * velScale + (velScale - 1)
        Vector(
          SupColorB(btn.light, () => if (state.velocity / velScale == idx) Color.whiteColor() else clip.color().get),
          EB(btn.st.press, velNote(vel), () => setVelocity(vel))
        )
      val noteBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn = j.matrix(row + 4)(col + 4)
        Vector(
          SupColorB(btn.light, clip.color()),
          EB(btn.st.press, "set note", () => ???)
        )
      override val modeBindings = velBindings.flatten ++ noteBindings.flatten
    }

    lazy val dpad = SimpleModeLayer("dpadStep", Vector(        
      EB(j.dpad.up.st.press, "scroll page up", () => scrollYinc(keyPageSize)),
      EB(j.dpad.down.st.press, "scroll page down", () => scrollYinc(-1 * keyPageSize)),
      EB(j.dpad.left.st.press, "", Noop),
      EB(j.dpad.right.st.press, "", Noop),
      SupBooleanB(j.dpad.up.light.isOn(), clip.canScrollKeysUp()),
      SupBooleanB(j.dpad.down.light.isOn(), clip.canScrollKeysDown()),
      SupBooleanB(j.dpad.left.light.isOn(), () => false),
      SupBooleanB(j.dpad.right.light.isOn(), () => false),
    ))

    override val subModes: Vector[ModeLayer] = Vector(
      stepMatrix,
      stepPages,
      patLength,
      gridSelect,
      chanSelect,
      velAndNote,
      dpad,
    )

    override def subModesToActivate = (Vector(stepMatrix, stepPages, dpad) ++ subModes.filter(_.isOn)).distinct

    override val modeBindings: Seq[Binding[_, _, _]] =
      Vector(
        EB(j.ShiftSolo.press, "shift-solo pressed", () => patLength.activateEvent),
        EB(j.ShiftSolo.releaseAll, "shift-solo released", () => patLength.deactivateEvent),
      )

    override val loadBindings: Seq[Binding[_, _, _]] = Vector(
      EB(j.step.st.press, "step toggle", () => toggleEvent),
      SupBooleanB(j.step.light.isOn, () => isOn),
    )
  }
}
