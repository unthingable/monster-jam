package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.controller.api.CursorTrack
import com.bitwig.extension.controller.api.Setting
import com.bitwig.extension.controller.api.Track

import com.github.unthingable.Util
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.jam.TrackTracker
import com.github.unthingable.jam.TrackId.apply
import com.github.unthingable.jam.TrackId
import com.github.unthingable.jam.stepSequencer.state.*

import scala.collection.mutable

trait TrackedState(selectedClipTrack: CursorTrack)(using
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
    import SeqState.toNoteName
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
        notify(s"Scale: ${toNoteName(newSt.scaleRoot)} ${newSt.scale.name}")
      if stateDiff(_.stepString) then notify(s"Step size: ${newSt.stepString}")
      if stateDiff(_.keyPageSize) || stateDiff(_.stepPageSize) then
        notify(s"Step grid: ${newSt.keyPageSize} x ${newSt.stepPageSize}")
}