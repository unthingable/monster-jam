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
import com.github.unthingable.framework.quant
import com.github.unthingable.jam.surface.KeyMaster.JC
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.stepSequencer.*

import scala.collection.mutable
import scala.collection.mutable.ArraySeq
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.BindingDSL
import com.github.unthingable.framework.Watched
import java.time.Instant
import com.github.unthingable.JamSettings.DpadScroll
import com.bitwig.`extension`.controller.api.Track
import com.github.unthingable.jam.TrackId.apply
import com.github.unthingable.jam.TrackId
import com.bitwig.`extension`.controller.api.Setting
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.jam.TrackTracker
import com.bitwig.`extension`.controller.api.CursorTrack
import com.github.unthingable.framework.binding.GlobalEvent
import com.github.unthingable.framework.binding.GlobalEvent.ClipSelected

trait TrackedState(selectedClipTrack: CursorTrack)(using
  ext: MonsterJamExt,
  tracker: TrackTracker
) {
  var ts                 = SeqState.empty // track state
  private val stateCache = mutable.HashMap.empty[TrackId, SeqState]

  private val bufferSize = 1024 * 1024
  private val stateStore = ext.document.getStringSetting("stepState", "MonsterJam", bufferSize, "")
  stateStore.asInstanceOf[Setting].hide()

  ext.application.projectName().addValueObserver(_ => restoreState())

  def storeState(): Unit =
    val data = Util.serialize(stateCache.toSeq)
    Util.println(
      s"saving stepState: ${data.size} chars, ${data.size.doubleValue() / bufferSize} of buffer"
    )
    stateStore.set(data)

  def restoreState(): Unit =
    Util
      .deserialize[Seq[(TrackId, SeqState)]](stateStore.get())
      .filterOrElse(_.nonEmpty, new Exception("Deserialized empty"))
      .left
      .map { e =>
        Util.println(s"Failed to deserialize step states: ${e}"); e
      }
      .foreach(data =>
        stateCache.clear()
        stateCache.addAll(data)
        ext.host.scheduleTask(() => updateState(ext.cursorTrack), 30)
      )

  def setState(st: SeqState) =
    val old = ts
    ts = st
    tracker.trackId(selectedClipTrack).foreach(stateCache.update(_, st))
    storeState()

  def updateState(cursorTrack: Track): Unit =
    val st = tracker
      .trackId(selectedClipTrack)
      .map(stateCache.getOrElseUpdate(_, SeqState.empty))
      .getOrElse(SeqState.empty)
    ts = st
    storeState()
}

trait StepSequencer extends BindingDSL { this: Jam =>
  val stepModeMap = Map(
    0 -> StepMode.One,
    1 -> StepMode.Two,
    // 1 -> StepMode.OneFull,
    3 -> StepMode.Four,
    7 -> StepMode.Eight
  )

  lazy val stepSequencer = new ModeCycleLayer("STEP")
    with ListeningLayer
    with TrackedState(selectedClipTrack) {
    val gridHeight               = 128
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
      .addValueObserver(v =>
        selectedClipTrack.selectChannel(ext.cursorTrack)
        // updateState(ext.cursorTrack)
        ext.host.scheduleTask(() => updateState(ext.cursorTrack), 30)
      )
    
    // follow clip selection
    ext.events.addSub((e: ClipSelected) =>
      if (isOn) selectedClipTrack.selectChannel(superBank.getItemAt(e.globalTrack))
      )

    Vector(
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
      selectedClipTrack.color,
      selectedClipTrack.position,
      selectedClipTrack.name,
      ext.transport.isPlaying,
      devices.itemCount(), // hopefully this gets updated
    ).foreach(_.markInterested())

    object localState:
      var stepState: Watched[StepState] = Watched(StepState(List.empty, false), onStepState)

    clip.setStepSize(ts.stepSize)
    // clip.scrollToKey(12 * 3)
    setGrid(StepMode.One)

    // def detectDrum(): Option[Device] = (0 until devices.itemCount().get()).map(devices.getDevice).find(_.hasDrumPads.get())

    /* Translate from matrix grid (row, col) to clip grid (x, y) */
    inline def m2clip(row: Int, col: Int): (Int, Int) =
      // no need to account for viewport as long as starts at 0,0
      val offset = row * 8 + col // matrix grid scanned
      val result = (
        offset % ts.stepPageSize,
        ts.keyScrollOffsetGuarded - (offset / ts.stepPageSize)
      )
      result

    def setGrid(mode: StepMode): Unit =
      setState(ts.copy(stepMode = mode))
      ext.host.showPopupNotification(s"Step grid: ${ts.keyPageSize} x ${ts.stepPageSize}")

    def incStepSize(inc: Short): Unit =
      setState(ts.copy(stepSizeIdx = (ts.stepSizeIdx + inc).min(quant.stepSizes.size - 1).max(0)))
      clip.setStepSize(ts.stepSize)
      ext.host.showPopupNotification(s"Step size: ${ts.stepString}")

    inline def scrollY(offset: Int) =
      setState(ts.copy(keyScrollOffset = ts.guardY(offset)))

    inline def scrollY(dir: UpDown, size: => Int): Unit =
      scrollYinc(dir match
        case UpDown.Up   => 1 * size
        case UpDown.Down => -1 * size
      )

    inline def scrollY(dir: UpDown): Unit = scrollY(dir, ts.keyPageSize)

    inline def scrollYinc(inc: Int) =
      scrollY(ts.keyScrollOffsetGuarded + inc)

    enum UpDown:
      case Up, Down

    inline def canScroll(dir: UpDown): Boolean =
      clip.exists.get() && (dir match
        case UpDown.Up   => ts.keyScrollOffsetGuarded + ts.stepViewPort.height < 127
        case UpDown.Down => ts.keyScrollOffsetGuarded + (8 - ts.stepViewPort.height) > 0
      )

    inline def setStepPage(page: Int) =
      setState(ts.copy(stepScrollOffset = ts.stepPageSize * page))
      clip.scrollToStep(ts.stepScrollOffset)

    inline def stepAt(x: Int, y: Int): NoteStep =
      clip.getStep(ts.channel, x, y)

    def stepPress(x: Int, y: Int): Unit =
      inline def hasStep: Boolean = stepAt(x, y).state == NSState.NoteOn

      val newState: StepState = localState.stepState.get match
        case StepState(Nil, _) =>
          val step = stepAt(x, y)
          if (step.state == NSState.Empty)
            clip.setStep(ts.channel, x, y, ts.velocity, ts.stepSize)
            StepState(List((Point(x, y), step)), true)
          else StepState(List((Point(x, y), step)), false)
        case st @ StepState(p @ (Point(x0, y0), _) :: Nil, _) if y == y0 && x > x0 && !hasStep =>
          val step: NoteStep = stepAt(x0, y0)
          val newDuration    = ts.stepSize * (x - x0 + 1)
          if (step.duration() == newDuration)
            step.setDuration(ts.stepSize)
          else
            step.setDuration(newDuration)
          st.copy(noRelease = true)
        case st @ StepState(steps, noRelease) if hasStep =>
          StepState(steps :+ (Point(x, y), stepAt(x, y)), noRelease)
        case st => st
      localState.stepState.set(newState)
      // Util.println(stepState.toString())

    def stepRelease(X: Int, Y: Int): Unit =
      val newState = localState.stepState.get match
        case StepState((Point(X, Y), _) :: Nil, noRelease) =>
          if (!noRelease)
            val step = stepAt(X, Y)
            step.state match
              case NSState.NoteOn => clip.clearStep(ts.channel, X, Y)
              case NSState.Empty | NSState.NoteSustain =>
                clip.setStep(ts.channel, X, Y, ts.velocity, ts.stepSize)
          StepState(List.empty, false)
        case st => st.copy(steps = st.steps.filter(_._1 != Point(X, Y)))
      localState.stepState.set(newState)
      // Util.println(stepState.toString())

    def clipColor: Color =
      if clip.exists().get then clip.color().get
      else selectedClipTrack.color().get

    lazy val stepMatrix = new SimpleModeLayer("stepMatrix") {
      override def onActivate(): Unit =
        super.onActivate()
        // state.stepState.set(StepState(List.empty, false))

      override val modeBindings: Seq[Binding[_, _, _]] =
        (for (col <- EIGHT; row <- EIGHT) yield {
          // val (stepNum, x, y) = stepView(row, col)
          def xy: (Int, Int) = m2clip(row, col)
          // def cachedClip = steps(channel)(xy._1)(xy._2)
          Vector(
            SupColorStateB(
              j.matrix(row)(col).light,
              () =>
                // chasing light
                if (
                  ext.transport.isPlaying
                    .get() && clip.playingStep().get() - ts.stepScrollOffset == xy._1
                ) // not right yet
                  JamColorState(JamColorBase.WHITE, 1)
                else {
                  clip.getStep(0, xy._1, xy._2).state() match {
                    case NSState.NoteOn      => JamColorState(clipColor, 1)
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

    lazy val stepPages = SimpleModeLayer(
      "stepPages",
      j.sceneButtons.zipWithIndex.flatMap { (btn, i) =>
        def hasContent = clip.getLoopLength().get() > i * ts.stepSize * ts.stepPageSize
        Vector(
          EB(btn.st.press, "", () => if (hasContent) setStepPage(i)),
          SupColorStateB(
            btn.light,
            () =>
              if (hasContent)
                // if (i == stepOffset / 32) Color.whiteColor() else clip.color().get()
                if (i == ts.stepScrollOffset / ts.stepPageSize)
                  JamColorState(JamColorBase.WHITE, 3)
                else if (clip.playingStep().get() / ts.stepPageSize == i)
                  JamColorState(JamColorBase.WHITE, 0)
                else JamColorState(clipColor, 1)
              else JamColorState.empty
          ),
        )
      }
    )

    lazy val stepEnc = SimpleModeLayer(
      "stepEnc",
      Vector(
        HB(
          j.encoder.turn,
          "note scroll",
          stepTarget(
            () => scrollY(UpDown.Up, 1),
            () => scrollY(UpDown.Down, 1)
          )
        )
      )
    )

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
              JamColorState(clipColor, 2)
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
                () =>
                  if (ts.stepMode == sm) JamColorState(Color.whiteColor(), 3)
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

    lazy val chanSelect = ModeButtonLayer(
      "chanSelect",
      j.perform,
      (for (row <- 0 until 4; col <- 0 until 4) yield
        val btn  = j.matrix(row + 4)(col + 4)
        val chan = (3 - row) * 4 + col
        Vector(
          SupColorB(btn.light, () => if (chan == ts.channel) Color.whiteColor else clipColor),
          EB(btn.st.press, s"select channel $chan", () => setChannel(chan))
        )
      ).flatten,
      GateMode.Gate
    )

    lazy val velAndNote = new ModeButtonLayer("velAndNote", j.notes) {
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

      def setVelocity(vel: Int) =
        ext.host.showPopupNotification(velNote(vel))
        setState(ts.copy(velocity = vel))
        selectedClipTrack.playNote(ts.keyScrollOffsetGuarded, vel)

      def notePress(note: Int): Unit =
        scrollY(note)
        selectedClipTrack.startNote(note, ts.velocity)

      def noteRelease(note: Int): Unit =
        selectedClipTrack.stopNote(note, ts.velocity)

      val velBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn             = j.matrix(row + 4)(col)
        inline val velScale = 8
        val idx             = row * 4 + col
        val vel             = idx * velScale + (velScale - 1)
        Vector(
          SupColorB(
            btn.light,
            () => if (ts.velocity / velScale == idx) Color.whiteColor() else clipColor
          ),
          EB(btn.st.press, velNote(vel), () => setVelocity(vel))
        )

      private val tmpPageOffset = 32
      val noteBindings = for (row <- 0 until 4; col <- 0 until 4) yield
        val btn     = j.matrix(row + 4)(col + 4)
        val noteIdx = tmpPageOffset + (3 - row) * 4 + col
        Vector(
          SupColorStateB(
            btn.light,
            () =>
              if (noteIdx == ts.keyScrollOffsetGuarded)
                JamColorState(Color.whiteColor(), 3)
              else
                JamColorState(
                  clipColor,
                  if (selectedClipTrack.playingNotes().isNotePlaying(noteIdx)) 3 else 0
                )
          ),
          EB(btn.st.press, "set note", () => notePress(noteIdx)),
          EB(btn.st.release, "release note", () => noteRelease(noteIdx)),
        )
      override val modeBindings = velBindings.flatten ++ noteBindings.flatten
    }

    lazy val dpadStep = SimpleModeLayer(
      "dpadStep",
      Vector(
        EB(j.dpad.up.st.press, "scroll page up", () => scrollY(UpDown.Up)),
        EB(j.dpad.down.st.press, "scroll page down", () => scrollY(UpDown.Down)),
        EB(j.dpad.left.st.press, "", Noop),
        EB(j.dpad.right.st.press, "", Noop),
        SupBooleanB(j.dpad.up.light.isOn(), () => canScroll(UpDown.Up)),
        SupBooleanB(j.dpad.down.light.isOn(), () => canScroll(UpDown.Down)),
        SupBooleanB(j.dpad.left.light.isOn(), () => false),
        SupBooleanB(j.dpad.right.light.isOn(), () => false),
      )
    )

    override val subModes: Vector[ModeLayer] = Vector(
      stepMatrix,
      stepPages,
      stepEnc,
      patLength,
      gridSelect,
      chanSelect,
      velAndNote,
      dpadStep
    )

    def onStepState(from: StepState, to: StepState): Unit = ()

    override def subModesToActivate =
      (Vector(stepMatrix, stepPages, stepEnc, dpadStep) ++ subModes.filter(_.isOn)).distinct

    override val modeBindings: Seq[Binding[_, _, _]] =
      Vector(
        EB(j.ShiftSolo.press, "shift-solo pressed", () => patLength.activateEvent),
        EB(j.ShiftSolo.releaseAll, "shift-solo released", () => patLength.deactivateEvent),
      ) ++ JCB.empty(j.song)

    override val loadBindings: Seq[Binding[_, _, _]] = Vector(
      EB(j.step.st.press, "step toggle", () => toggleEvent),
      SupBooleanB(j.step.light.isOn, () => isOn),
    )

    override def onActivate(): Unit =
      restoreState()
      super.onActivate()
  }
}

/* todos
[x] store grid settings per track
[ ] autoscroll to content when there isn't any
[x] knob scrolls notes
[ ] note pages
- step-hold to select or create clips before entering sequencer
 */
