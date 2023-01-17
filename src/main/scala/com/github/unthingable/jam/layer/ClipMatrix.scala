package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.Clip
import com.bitwig.extension.controller.api.ClipLauncherSlot
import com.bitwig.extension.controller.api.ClipLauncherSlotBank
import com.bitwig.extension.controller.api.Track
import com.github.unthingable.Util
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.framework.quant
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.KeyMaster.JC

import java.time.Duration
import java.time.Instant
import scala.collection.mutable

given Util.SelfEqual[ClipLauncherSlot] = CanEqual.derived

trait ClipMatrix:
  this: Jam =>
  lazy val clipMatrix = new SimpleModeLayer("clipMatrix"):
    case class PressedAt(var value: Instant)
    val cursorClip: Clip = ext.host.createLauncherCursorClip(0, 0)
    cursorClip.launchQuantization().markInterested()
    trackBank.sceneBank().setIndication(true)
    trackBank.setShouldShowClipLauncherFeedback(true)
    ext.transport.playPosition().markInterested()
    ext.transport.defaultLaunchQuantization().markInterested()
    ext.preferences.launchTolerance.markInterested()
    ext.transport.timeSignature().numerator().markInterested()
    ext.transport.timeSignature().denominator().markInterested()
    ext.transport.tempo().markInterested()

    override val modeBindings: Seq[Binding[?, ?, ?]] = j.matrix.indices.flatMap { col =>
      val track = trackBank.getItemAt(col)
      track.isQueuedForStop.markInterested()

      val clips = track.clipLauncherSlotBank()

      val pressedAt: mutable.Seq[PressedAt] = mutable.ArraySeq.fill(8)(PressedAt(Instant.now()))

      EIGHT.flatMap { row =>
        val btn  = j.matrix(row)(col)
        val clip = clips.getItemAt(row)
        clip.color().markInterested()
        clip.hasContent.markInterested()
        clip.isPlaying.markInterested()
        clip.isRecording().markInterested()
        clip.isRecordingQueued().markInterested()
        clip.isSelected.markInterested()
        clip.isPlaybackQueued.markInterested()
        clip.isStopQueued.markInterested()
        clip.name.markInterested()
        clips.exists().markInterested()

        Vector(
          SupColorStateB(btn.light, () => clipColor(track, clip), JamColorState.empty),
          EB(
            btn.st.press,
            s"clipPress $row:$col",
            () => handleClipPress(clip, clips, pressedAt(col))
          ),
          EB(
            btn.st.release,
            s"clipRelease $row:$col",
            () => handleClipRelease(clip, clips, pressedAt(col))
          ),
        )
      }
    } ++ Vector(
      // FIXME: last released combo button needs to be handled as combo, not single - fixed?
      EB(
        j.duplicate.st.release,
        "dup clips: clear source",
        () => source = None,
        BB(tracked = false, managed = false)
      ),
      EB(j.Combo.Shift.duplicate.press, "dup clips: duplicate content", () => cursorClip.duplicateContent)
    )

    // for duplication
    private var source: Option[ClipLauncherSlot] = None

    private def handleClipPress(
      clip: ClipLauncherSlot,
      clips: ClipLauncherSlotBank,
      pressedAt: PressedAt
    ): Unit =
      /* Until we're able to directly access clips, must rely on cursor, so select always */
      clip.select()
      if GlobalMode.Select.isOn then () // clip.select()
      else if GlobalMode.Clear.isOn then clip.deleteObject()
      else if GlobalMode.Duplicate.isOn then
        if source.isEmpty then source = Some(clip)
        else
          source.foreach(s => if s != clip then clip.replaceInsertionPoint().copySlotsOrScenes(s))
          source = None
      else pressedAt.value = Instant.now()

    private def handleClipRelease(
      clip: ClipLauncherSlot,
      clips: ClipLauncherSlotBank,
      pressedAt: PressedAt
    ): Unit =
      if Instant.now().isAfter(pressedAt.value.plus(Duration.ofSeconds(1))) then () // clip.select() -- see above
      else if clip.isPlaying.get() && ext.transport.isPlaying.get() then clips.stop()
      else
        launchOptions(clip) match
          case None => clip.launch()
          case Some((quant, mode)) =>
            Util.println(s"lenient launch $quant $mode")
            clip.launchWithOptions(quant, mode)
          // Util.println(s"launch fired at ${ext.transport.playPosition().get()}")

    /* If we're a little late starting the clip, that's ok */
    private def launchOptions(clip: ClipLauncherSlot): Option[(String, String)] =
      val launchTolerance: Double = ext.preferences.launchTolerance.get()
      val lookAhead: Double       = ext.preferences.launchLookahead.get()
      val clipQString: String     = cursorClip.launchQuantization().get()
      val qString: String =
        if clipQString == "default" then ext.transport.defaultLaunchQuantization().get()
        else clipQString
      // Util.println(s"tempo: ${ext.transport.tempo().get()}")
      if launchTolerance == 0 || qString == "none" then None
      else
        quant.gridDistanceWithNow(qString) match
          case None =>
            Util.println(s"Unparseable quant: $qString")
            None
          case Some((prev, now, next)) =>
            // Util.println(s"lenient calc: $qString $beat $qSize ${beat % (qSize * 4)}")
            Util.println(s"lenient calc: $clipQString $qString $prev $now $next")
            if prev < launchTolerance || (lookAhead > 0 && next < lookAhead) then
              Some(clipQString, "continue_immediately")
            else None
    end launchOptions

    private def clipColor(track: Track, clip: ClipLauncherSlot): JamColorState =
      if GlobalMode.Select.isOn && clip.isSelected.get() then JamColorState(JamColorBase.WHITE, 3)
      else if !GlobalMode.Select.isOn && source.contains(clip) then
        JamColorState(JamColorBase.WHITE, if j.Mod.blink then 3 else 1)
      else
        JamColorState(
          if clip.hasContent.get() then JamColorState.toColorIndex(clip.color().get())
          else if clip.isRecordingQueued().get() then JamColorBase.RED
          else JamColorBase.OFF,
          brightness =
            if clip.isPlaying.get() then
              if track.isQueuedForStop.get() then if j.Mod.blink then 3 else -1
              else 3
            else if clip.isPlaybackQueued.get() || clip.isRecordingQueued().get() then if j.Mod.blink then 0 else 3
            else 0
        )
end ClipMatrix
