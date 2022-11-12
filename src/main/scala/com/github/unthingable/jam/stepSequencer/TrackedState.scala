package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.CursorTrack
import com.bitwig.extension.controller.api.DeviceBank
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.PinnableCursorClip
import com.bitwig.extension.controller.api.Setting
import com.bitwig.extension.controller.api.Track

import com.github.unthingable.Util
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.quant
import com.github.unthingable.jam.TrackTracker
import com.github.unthingable.jam.TrackId.apply
import com.github.unthingable.jam.TrackId
import com.github.unthingable.jam.stepSequencer.state.*

import scala.collection.mutable
import com.github.unthingable.framework.Watched

trait TrackedState(val selectedClipTrack: CursorTrack)(using
  ext: MonsterJamExt,
  tracker: TrackTracker
) { this: ModeLayer =>
  private var _ts        = SeqState.empty // track state
  private val stateCache = mutable.HashMap.empty[TrackId, SeqState]

  private val bufferSize = 1024 * 1024
  private val stateStore = ext.document.getStringSetting("stepState", "MonsterJam", bufferSize, "")
  stateStore.asInstanceOf[Setting].hide()

  ext.application.projectName().addValueObserver(_ => restoreState())

  def ts = _ts

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
    val old = _ts
    _ts = st
    tracker.trackId(selectedClipTrack).foreach(stateCache.update(_, st))
    storeState()
    echoStateDiff(old, st)

  def updateState(cursorTrack: Track): Unit =
    val st = tracker
      .trackId(selectedClipTrack)
      .map(stateCache.getOrElseUpdate(_, SeqState.empty))
      .getOrElse(SeqState.empty)
    echoStateDiff(ts, st)
    _ts = st
    // storeState()

  def comparator[A, B](a: A, b: A)(f: A => B): Boolean =
    f(a) == f(b)

  def echoStateDiff(oldSt: SeqState, newSt: SeqState) =
    import SeqState.{toNoteName, toNoteNameNoOct}
    // can't show multiple notifications at once, oh well
    val stateDiff = comparator(oldSt, newSt) andThen (_.unary_!)
    val notify    = ext.host.showPopupNotification
    if (isOn)
      if stateDiff(_.keyScrollOffset) || stateDiff(_.keyPageSize) then
        val notes =
          newSt.keyScrollOffset +: (
            if newSt.keyPageSize > 1 then
              Seq(newSt.fromScale((newSt.keyScrollOffset.value + 1 + newSt.keyPageSize).asInstanceOf[ScaledNote]))
            else Seq()
          )
        notify(s"Notes: ${notes.map(toNoteName).mkString(" - ")}")
      if stateDiff(_.scale) || stateDiff(_.scaleRoot) then
        notify(s"Scale: ${toNoteNameNoOct(newSt.scaleRoot)} ${newSt.scale.name}")
      if stateDiff(_.stepString) then notify(s"Step size: ${newSt.stepString}")
      if stateDiff(_.keyPageSize) || stateDiff(_.stepPageSize) then
        notify(s"Step grid: ${newSt.keyPageSize} x ${newSt.stepPageSize}")
}

trait StepCap(using MonsterJamExt, TrackTracker) extends TrackedState, ModeLayer {
  val gridHeight               = 128
  val gridWidth                = 64
  val fineRes                  = 128
  val clip: PinnableCursorClip = selectedClipTrack.createLauncherCursorClip(gridWidth, gridHeight)
  val fineClip                 = selectedClipTrack.createLauncherCursorClip(gridWidth * fineRes, gridHeight)
  val secondClip               = selectedClipTrack.createLauncherCursorClip(1, 1)

  val devices: DeviceBank = selectedClipTrack.createDeviceBank(1)
  lazy val colorManager   = ColorManager(clipColor)

  object localState:
    var stepState: Watched[StepState] = Watched(StepState(List.empty, false), onStepState)
    val selectedClips                 = mutable.HashMap.empty[Int, Int]
  
  def onStepState(from: StepState, to: StepState): Unit
  
  def clipColor: Color =
    if clip.exists().get then clip.color().get
    else selectedClipTrack.color().get

  /* Translate from matrix grid (row, col) to clip grid (x, y) */
  inline def m2clip(row: Int, col: Int): (Int, Int) =
    // no need to account for viewport as long as starts at 0,0
    val offset = row * 8 + col // matrix grid scanned
    val result = (
      offset % ts.stepPageSize,
      ts.scale.fullScale(ts.keyScrollOffsetGuarded.value + (offset / ts.stepPageSize)).value
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

  inline def scrollYTo(y: ScaledNote) =
    setState(ts.copy(keyScrollOffset = ts.guardY(ts.fromScale(y))))

  inline def scrollYBy(offset: Int) = // offset in scaled notes
    scrollYTo(
      ts.toScale((ts.keyScrollOffsetGuarded.value + offset).asInstanceOf[RealNote])
    )

  inline def scrollYBy(dir: UpDown, size: => Int): Unit =
    scrollYBy(size * (inline dir match
      case UpDown.Up   => 1
      case UpDown.Down => -1
    ))

  inline def scrollYPage(dir: UpDown): Unit = scrollYBy(dir, ts.keyPageSize)

  enum UpDown:
    case Up, Down

  inline def canScrollY(dir: UpDown): Boolean =
    clip.exists.get() && (inline dir match
      case UpDown.Down => ts.scale.notesRemainingUp(ts.keyScrollOffsetGuarded).exists(_ > ts.stepViewPort.height)
      case UpDown.Up   => ts.scale.notesRemainingDown(ts.keyScrollOffsetGuarded).exists(_ > 0)
    )

  inline def setStepPage(page: Int) =
    scrollXTo(ts.stepPageSize * page)

  def scrollXTo(offset: Int) =
    setState(ts.copy(stepScrollOffset = offset))
    clip.scrollToStep(ts.stepScrollOffset)
    fineClip.scrollToStep(ts.stepScrollOffset * fineRes)

  def scrollXBy(inc: Int) = scrollXTo(ts.stepScrollOffset + inc)

  inline def scrollXBy(dir: UpDown, size: => Int): Unit =
    scrollXBy(size * (inline dir match
      case UpDown.Up   => 1
      case UpDown.Down => -1
    ))

  inline def stepAt(x: Int, y: Int): NoteStep =
    clip.getStep(ts.channel, x, y)

  // FIXME scan the visible count of steps for the first available step
  def findStep(): NoteStep =
    ???
}
