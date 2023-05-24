package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.CursorTrack
import com.bitwig.extension.controller.api.DeviceBank
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.PinnableCursorClip
import com.bitwig.extension.controller.api.Setting
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.Watched
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.quant
import com.github.unthingable.jam.TrackId
import com.github.unthingable.jam.TrackTracker
import com.github.unthingable.jam.stepSequencer.state.*

import scala.collection.mutable

import Util.trace

transparent trait TrackedState(val selectedClipTrack: CursorTrack)(using
  ext: MonsterJamExt,
  tracker: TrackTracker
):
  this: ModeLayer =>
  private var _ts                   = SeqState.empty(None) // track state
  private var _tid: Option[TrackId] = None
  private val stateCache            = mutable.HashMap.empty[TrackId, SeqState]

  private val bufferSize = 1024 * 1024
  private val stateStore = ext.document.getStringSetting("stepState", "MonsterJam", bufferSize, "")
  stateStore.asInstanceOf[Setting].hide()

  ext.application.projectName().addValueObserver(_ => restoreState())
  selectedClipTrack.position.addValueObserver(pos =>
    val tid = tracker.trackId(selectedClipTrack) // .trace(s"TrackedState: caching id for $pos")
    _tid = tid
    updateState(tid)
  )

  /** SeqState for the current track */
  def ts: SeqState = _ts

  /** Handle SeqState change on the current track */
  def onSeqState(oldSt: SeqState, newSt: SeqState): Unit

  /** Serialize and save current sequencer states in project */
  def storeState(): Unit =
    val data = Util.serialize(stateCache.toSeq)
    stateStore.set(data)

  /** Deserialize and materialize sequencer states from project (when switching) */
  def restoreState(): Unit =
    Util
      .deserialize[Seq[(TrackId, SeqState)]](stateStore.get())
      .filterOrElse(_.nonEmpty, new Exception("Deserialized empty"))
      .left
      .map(_.trace("Failed to deserialize step states:"))
      // .map(_.trace("Deserialized state"))
      .foreach(data =>
        stateCache.clear()
        stateCache.addAll(
          data.map((tid, st) => (tid, Util.fillNull(st, SeqState.empty(Some(tid)))))
        )
        ext.host.scheduleTask(() => updateState(_tid), 20)
      )

  /** Commit SeqState of the currently selected track and save it */
  def setState(st: SeqState): Unit =
    // protect against a state update coming in too late
    if st.tid == _tid then
      val old = _ts
      _ts = st
      _tid
        // .map(_.trace(s => s"setState for $s $st"))
        .foreach(stateCache.update(_, st))
      storeState()
      echoStateDiff(old, st)
      onSeqState(old, st)
    else st.trace(s"setState mismatch, tid is now $_tid")

  /** When switching to a new track, pull its SeqState from cache and make it current */
  private def updateState(id: Option[TrackId]): Unit =
    val st: SeqState = id
      .map(tid => stateCache.getOrElseUpdate(tid, SeqState.empty(Some(tid))))
      .getOrElse(SeqState.empty(None))
    echoStateDiff(ts, st)
    onSeqState(ts, st)
    _ts = st

  /** True if SeqState and current track are still in sync */
  def isCurrent(ts: SeqState): Boolean = ts.tid == _tid

  /** Display appropriate notification when state changes */
  def echoStateDiff(oldSt: SeqState, newSt: SeqState) =
    import SeqState.{toNoteName, toNoteNameNoOct}
    // can't show multiple notifications at once, oh well
    val stateDiff     = Util.comparator(oldSt, newSt) andThen (_.unary_!)
    val notifications = mutable.ArrayDeque.empty[String]
    val notify        = notifications.addOne
    if isOn then
      if stateDiff(_.keyScaledOffset) || stateDiff(_.keyPageSize) then
        val notes =
          newSt.keyScaledOffset +:
            (
              if newSt.keyPageSize > 1 then
                Seq((newSt.keyScaledOffset.asInstanceOf[Int] + 1 + newSt.keyPageSize).asInstanceOf[ScaledNote])
              else Seq()
            )
        notify(s"Notes: ${notes.map(n => toNoteName(ts.fromScale(n))).mkString(" - ")}")
      if stateDiff(_.scaleIdx) || stateDiff(_.scaleRoot) then
        notify(s"Scale: ${toNoteNameNoOct(newSt.scaleRoot)} ${newSt.scale.name}")
      if stateDiff(_.stepString) then notify(s"Step size: ${newSt.stepString}")
      if stateDiff(_.keyPageSize) || stateDiff(_.stepPageSize) then
        notify(s"Step grid: ${newSt.keyPageSize}x${newSt.stepPageSize}")

      if notifications.nonEmpty then ext.host.showPopupNotification(notifications.mkString(" "))
  end echoStateDiff
end TrackedState

/** A collection of utility methods useful for working with sequencer steps */
transparent trait StepCap(using MonsterJamExt, TrackTracker) extends TrackedState, ModeLayer:
  val gridHeight               = 128
  val gridWidth                = 64
  val fineRes                  = 128
  val clip: PinnableCursorClip = selectedClipTrack.createLauncherCursorClip(gridWidth, gridHeight)
  val fineClip                 = selectedClipTrack.createLauncherCursorClip(gridWidth * fineRes, gridHeight)
  val secondClip               = selectedClipTrack.createLauncherCursorClip(1, 1)

  protected var rowSelected: Option[Int] = None

  val devices: DeviceBank = selectedClipTrack.createDeviceBank(1)
  lazy val colorManager   = ColorManager(clipColor)

  /** Local state doesn't need to be stored */
  object localState:
    var stepState: Watched[StepState] = Watched(StepState(List.empty, false), onStepState)
    val selectedClips                 = mutable.HashMap.empty[Int, Int]

  /** Overrideable function to handle changes to selected steps */
  def onStepState(from: StepState, to: StepState): Unit

  def clipColor: Color =
    if clip.exists().get then clip.color().get
    else selectedClipTrack.color().get

  /* Translate from matrix grid (row, col) to clip grid (x, y) */
  def m2clip(row: Int, col: Int)(using ext: MonsterJamExt): Option[(Int, Int)] =
    val port = ts.stepViewPort
    if row < port.rowTop || row >= port.rowBottom || col < port.colLeft || col >= port.colRight then None
    else
      val offsetRow: Int = row - port.rowTop
      val offsetCol: Int = col - port.colLeft
      val offset: Int    = offsetRow * port.width + offsetCol // unrolled sequence
      val (step, note) =
        if ext.preferences.stepNoteInterlace.get() then
          // interlace by note
          (
            offset % ts.stepPageSize,
            ts.keyPageSize - offset / ts.stepPageSize
          )
        else
          val subpage: Int       = offsetRow / ts.keyPageSize
          val subpageOffset: Int = subpage * port.width
          (subpageOffset + offsetCol, ts.keyPageSize - offsetRow % ts.keyPageSize)
      Some(
        (step, ts.fromScale((ts.keyScrollOffsetGuarded.asInstanceOf[Int] + note - 1).asInstanceOf[ScaledNote]).value)
      )
  end m2clip

  def setGrid(mode: StepMode): Unit =
    setState(ts.copy(stepMode = mode))

  def incStepSize(inc: Short): Unit =
    setState(ts.copy(stepSizeIdx = (ts.stepSizeIdx + inc).min(quant.stepSizes.size - 1).max(0)))

  inline def scrollYTo(y: ScaledNote) =
    setState(ts.copy(keyScaledOffset = ts.guardYScaled(y)))

  inline def scrollYBy(offset: Int) = // offset in scaled notes
    scrollYTo((ts.keyScrollOffsetGuarded.asInstanceOf[Int] + offset).asInstanceOf[ScaledNote])

  inline def scrollYBy(dir: UpDown, size: => Int): Unit =
    scrollYBy(
      size *
        (inline dir match
          case UpDown.Up   => 1
          case UpDown.Down => -1
        )
    )

  inline def scrollYPage(dir: UpDown): Unit = scrollYBy(dir, ts.keyPageSize)

  enum UpDown derives CanEqual:
    case Up, Down

  inline def canScrollY(dir: UpDown): Boolean =
    clip.exists.get() &&
      (inline dir match
        case UpDown.Up   => ts.scale.length - ts.keyScrollOffsetGuarded.asInstanceOf[Int] > ts.stepViewPort.height
        case UpDown.Down => ts.keyScrollOffsetGuarded.asInstanceOf[Int] > 0
      )

  inline def setStepPage(page: Int) =
    scrollXTo(ts.stepPageSize * page)

  def scrollXTo(offset: Int) =
    setState(ts.copy(stepScrollOffset = offset))

  def scrollXBy(inc: Int) = scrollXTo(ts.stepScrollOffset + inc)

  inline def scrollXBy(dir: UpDown, size: => Int): Unit =
    scrollXBy(
      size *
        (inline dir match
          case UpDown.Up   => 1
          case UpDown.Down => -1
        )
    )

  inline def stepAt(x: Int, y: Int): NoteStep =
    clip.getStep(ts.channel, x, y)

  // FIXME scan the visible count of steps for the first available step
  def findStep(): NoteStep =
    ???

  // override def validateState(st: SeqState): SeqState =
  //   if !st.scale.isInScale(st.keyScrollOffset) then
  //     st.copy(keyScrollOffset =
  //       st.scale.prevInScale(st.keyScrollOffset).orElse(st.scale.nextInScale(st.keyScrollOffset)).get
  //     )
  //   else st
end StepCap
