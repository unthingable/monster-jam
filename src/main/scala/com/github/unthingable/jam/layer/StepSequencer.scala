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
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.ModeButtonCycleLayer
import com.github.unthingable.framework.mode.CycleMode
import com.github.unthingable.jam.SliderBankMode
import com.bitwig.`extension`.controller.api.Parameter
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.JamParameter
import com.github.unthingable.jam.SliderOp
import com.github.unthingable.framework.GetSetProxy
import com.bitwig.`extension`.controller.api.NoteOccurrence
import com.bitwig.`extension`.controller.api.Clip

trait TrackedState(selectedClipTrack: CursorTrack)(using
  ext: MonsterJamExt,
  tracker: TrackTracker
) { this: ModeLayer =>
  var ts                 = SeqState.empty // track state
  private val stateCache = mutable.HashMap.empty[TrackId, SeqState]

  private val bufferSize = 1024 * 1024
  private val stateStore = ext.document.getStringSetting("stepState", "MonsterJam", bufferSize, "")
  stateStore.asInstanceOf[Setting].hide()

  ext.application.projectName().addValueObserver(_ => restoreState())

  def storeState(): Unit =
    val data = Util.serialize(stateCache.toSeq)
    // Util.println(
    //   s"saving stepState: ${data.size} chars, ${data.size.doubleValue() / bufferSize} of buffer"
    // )
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
    echoStateDiff(old, st)

  def updateState(cursorTrack: Track): Unit =
    val st = tracker
      .trackId(selectedClipTrack)
      .map(stateCache.getOrElseUpdate(_, SeqState.empty))
      .getOrElse(SeqState.empty)
    echoStateDiff(ts, st)
    ts = st
    // storeState()

  def echoStateDiff(oldSt: SeqState, newSt: SeqState) =
    // can't show multiple notifications at once, oh well
    if (isOn)
      if (oldSt.stepString != newSt.stepString)
        ext.host.showPopupNotification(s"Step size: ${newSt.stepString}")
      if ((oldSt.keyPageSize, oldSt.stepPageSize) != (newSt.keyPageSize, newSt.stepPageSize))
        ext.host.showPopupNotification(s"Step grid: ${newSt.keyPageSize} x ${newSt.stepPageSize}")
}

trait StepSequencer extends BindingDSL { this: Jam =>
  val stepModeMap = Map(
    0 -> StepMode.One,
    1 -> StepMode.Two,
    // 1 -> StepMode.OneFull,
    3 -> StepMode.Four,
    7 -> StepMode.Eight
  )

  object stepSequencer
      extends ModeCycleLayer("STEP")
      with ListeningLayer
      with TrackedState(selectedClipTrack) {
    val gridHeight               = 128
    val gridWidth                = 64
    val fineRes                  = 128
    val clip: PinnableCursorClip = selectedClipTrack.createLauncherCursorClip(gridWidth, gridHeight)
    val fineClip   = selectedClipTrack.createLauncherCursorClip(gridWidth * fineRes, gridHeight)
    val secondClip = selectedClipTrack.createLauncherCursorClip(1, 1)
    val devices: DeviceBank = selectedClipTrack.createDeviceBank(1)

    // a mirror of the bitwig clip, channel / x / y
    val steps =
      ArraySeq.fill(16)(ArraySeq.fill(gridWidth)(ArraySeq.fill(gridHeight)(null: NoteStep)))

    clip.addNoteStepObserver(ns => steps(ns.channel())(ns.x()).update(ns.y(), ns))

    devices.itemCount().addValueObserver(v => Util.println(v.toString))
    clip.getPlayStop.addValueObserver(v => Util.println(s"beats $v"))
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
      localState.selectedSteps.update(e.globalTrack, e.globalClip)
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
      selectedClipTrack.color,
      selectedClipTrack.position,
      selectedClipTrack.name,
      ext.transport.isPlaying,
      devices.itemCount(), // hopefully this gets updated
    ).foreach(_.markInterested())

    object localState:
      var stepState: Watched[StepState] = Watched(StepState(List.empty, false), onStepState)
      val selectedSteps                 = mutable.HashMap.empty[Int, Int]

    clip.setStepSize(ts.stepSize)
    fineClip.setStepSize(ts.stepSize / fineRes.toDouble)
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
      // ext.host.showPopupNotification(s"Step grid: ${ts.keyPageSize} x ${ts.stepPageSize}")

    def incStepSize(inc: Short): Unit =
      setState(ts.copy(stepSizeIdx = (ts.stepSizeIdx + inc).min(quant.stepSizes.size - 1).max(0)))
      // should probably to this in onStepState
      clip.setStepSize(ts.stepSize)
      fineClip.setStepSize(ts.stepSize / fineRes.toDouble)
      // ext.host.showPopupNotification(s"Step size: ${ts.stepString}")

    inline def scrollY(offset: Int) =
      setState(ts.copy(keyScrollOffset = ts.guardY(offset)))

    inline def scrollY(dir: UpDown, size: => Int): Unit =
      scrollYinc(size * (inline dir match
        case UpDown.Up   => 1
        case UpDown.Down => -1
      ))

    inline def scrollY(dir: UpDown): Unit = scrollY(dir, ts.keyPageSize)

    inline def scrollYinc(inc: Int) =
      scrollY(ts.keyScrollOffsetGuarded + inc)

    enum UpDown:
      case Up, Down

    inline def canScroll(dir: UpDown): Boolean =
      clip.exists.get() && (inline dir match
        case UpDown.Up   => ts.keyScrollOffsetGuarded + ts.stepViewPort.height < 127
        case UpDown.Down => ts.keyScrollOffsetGuarded + (8 - ts.stepViewPort.height) > 0
      )

    inline def setStepPage(page: Int) =
      setState(ts.copy(stepScrollOffset = ts.stepPageSize * page))
      clip.scrollToStep(ts.stepScrollOffset)
      fineClip.scrollToStep(ts.stepScrollOffset * fineRes)

    inline def stepAt(x: Int, y: Int): NoteStep =
      clip.getStep(ts.channel, x, y)

    def stepPress(x: Int, y: Int): Unit =
      inline def hasStep: Boolean = stepAt(x, y).state == NSState.NoteOn

      val newState: StepState =
        val step  = stepAt(x, y)
        val pstep = PointStep(Point(x, y), step, Instant.now())
        localState.stepState.get match
          case StepState(Nil, _) =>
            if (step.state == NSState.Empty)
              clip.setStep(ts.channel, x, y, ts.velocity, ts.stepSize)
              StepState(List(PointStep(Point(x, y), stepAt(x, y), Instant.now())), true)
            else StepState(List(pstep), false)
          case st @ StepState(p @ PointStep(Point(x0, y0), _, _) :: Nil, _)
              if y == y0 && x > x0 && !hasStep =>
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

    lazy val notePages =
      new ModeButtonLayer("notePages", j.notes, gateMode = GateMode.Gate, silent = true) {
        override def modeBindings: Seq[Binding[?, ?, ?]] =
          j.sceneButtons.zipWithIndex.flatMap((btn, idx) =>
            Vector(
              EB(btn.st.press, "", () => ()),
              SupColorStateB(btn.light, () => JamColorState(JamColorBase.CYAN, 3))
            )
          )
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

    object tune
        extends ModeButtonCycleLayer(
          "step TUNE",
          j.tune,
          CycleMode.Select,
          gateMode = GateMode.Auto
        ) {

      case class FineStep(step: NoteStep, fineStep: NoteStep):
        def offset: Int = fineStep.x % fineRes
        def moveFineBy(clip: Clip, dx: Int): Unit =
          // ensure no crossings
          if (fineStep.x / 128 == (fineStep.x + dx) / 128)
            clip.moveStep(ts.channel, fineStep.x, fineStep.y, dx, 0)

      def toFine(step: NoteStep): Option[FineStep] =
        (0 until fineRes).view
          .map(i => fineClip.getStep(ts.channel, step.x * fineRes + i, step.y))
          .find(_.state() == NSState.NoteOn)
          .map(s => FineStep(step, s))

      import GetSetProxy.given
      val P = GetSetProxy[NoteStep, Double](0)

      val proxies: Vector[Option[GetSetProxy[NoteStep, Double]]] = Vector(
        // -- expressions
        P(_.velocity(), (s, v, _) => s.setVelocity(v)),
        P(_.releaseVelocity(), (s, v, _) => s.setReleaseVelocity(v)),
        P(_.velocitySpread(), (s, v, _) => s.setVelocitySpread(v)),
        // note start
        P(toFine(_).map(_.offset).getOrElse(0) / 128.0, (s, _, d) => toFine(s).foreach(_.moveFineBy(fineClip, (d * 128).toInt))),
        P(_.duration(), (s, v, _) => s.setDuration(v)),
        P(_.pan(), (s, v, _) => s.setPan(v)),
        P(_.timbre(), (s, v, _) => s.setTimbre(v)),
        P(_.pressure(), (s, v, _) => s.setPressure(v)),
        // -- operators
        P(_.chance(), (s, v, _) => s.setChance(v)),
        // P(_.occurrence().ordinal() / NoteOccurrence.values().length.toDouble, (s, v) => s.setOccurrence(NoteOccurrence.values.apply((v * 10).toInt))),
        // P(_.isRecurrenceEnabled(), (s, v) => s.setChance(v)),
        // P(_.recurrenceLength() / 9.0, (s, v) =>
        //   val rec = (v * 9).toInt
        //   if (rec == 0)
        //     s.setIsRecurrenceEnabled(false)
        //   else
        //     s.setIsRecurrenceEnabled(true)
        //     s.setRecurrence
        //   ),
        (), // occurence
        (), // recurrence
        (), // recurrence
        P(_.repeatCount(), (s, v, _) => s.setChance(v)),
      ).map {
        case p: GetSetProxy[NoteStep, Double] @unchecked => Some(p)
        case _: Unit                                     => None
      }

      val realProxies = proxies.flatten
      val expProxies  = proxies.slice(0, 8)
      val opProxies   = proxies.slice(8, 16)
      def proxiesForState = ts.expMode match
        case ExpMode.Exp      => expProxies
        case ExpMode.Operator => opProxies

      def mask = proxiesForState.map(_.isDefined)

      def setCurrentSteps(): Unit =
        setCurrentSteps(localState.stepState.get.steps.view.map(_.step))

      def setCurrentSteps(steps: Iterable[NoteStep]): Unit =
        if (steps.nonEmpty)
          Util.println(s"setting active $mask")
          realProxies.foreach(_.setTarget(steps))
          j.stripBank.setActive(mask)
          proxiesForState
            .zip(sliders.sliderOps)
            .foreach((p, s) => p.foreach(realp => s.set(realp.get, SliderOp.Source.Internal)))
        else
          realProxies.foreach(_.clearTarget())
          Util.println("setting inactive")
          sliders.sliderOps.foreach(_.set(0, SliderOp.Source.Internal))
          j.stripBank.setActive(_ => false)

      val callbacks: Vector[Option[Double => Unit]] =
        proxies.map(_.map(_.set))

      val sliders = new SliderBankMode(
        "note exp",
        callbacks,
        _.map(JamParameter.Internal.apply).getOrElse(JamParameter.Empty),
        Seq.fill(8)(BarMode.SINGLE),
        stripColor = Some(Util.rainbow)
      ):
        override def onActivate(): Unit =
          super.onActivate()
          setCurrentSteps()

      override def onActivate(): Unit =
        super.onActivate()
        select(0)
      override val subModes: Vector[ModeLayer] = Vector(sliders)
    }

    override val subModes: Vector[ModeLayer] = Vector(
      stepMatrix,
      stepPages,
      stepEnc,
      patLength,
      gridSelect,
      chanSelect,
      velAndNote,
      notePages,
      dpadStep,
      tune,
    )

    def onStepState(from: StepState, to: StepState): Unit =
      if (tune.isOn)
        val prev = from.steps.map(_.step)
        val curr = to.steps.map(_.step)
        if (prev != curr)
          tune.setCurrentSteps(curr)

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

    // TODO refactor this already
    // Find the first existing clip on a track
    def findClip(): Option[Clip] =
      secondClip.selectFirst()
      var cnt = 0
      while (cnt < 64 && !secondClip.exists().get())
        cnt += 1
        secondClip.selectNext()
      if (secondClip.exists().get()) Some(secondClip) else None

    override def onActivate(): Unit =
      restoreState()
      super.onActivate()

      if (!clip.exists().get())
        findClip() match
          case None =>
            val t = selectedClipTrack.position().get()
            val c = localState.selectedSteps.getOrElse(t, 0)
            selectedClipTrack.createNewLauncherClip(c)
            trackBank.getItemAt(t).clipLauncherSlotBank().select(c)
          case Some(foundClip) =>
            clip.selectClip(foundClip)
            clip.clipLauncherSlot().select()
  }
}

/* todos and ideas
[x] store grid settings per track
[ ] autoscroll to content when there isn't any
[x] knob scrolls notes
[ ] note pages
- step-hold to select or create clips before entering sequencer
- adjust mode (by holding aux? tune?)
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
