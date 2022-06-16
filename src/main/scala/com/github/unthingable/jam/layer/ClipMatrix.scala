package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{Clip, ClipLauncherSlot, ClipLauncherSlotBank, Track}
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.framework.binding.{Binding, EB, SupColorStateB, BindingBehavior => BB}
import com.github.unthingable.jam.surface.KeyMaster.JC
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState

import java.time.{Duration, Instant}
import scala.collection.mutable
import com.github.unthingable.framework.binding.EB

trait ClipMatrix { this: Jam =>
  lazy val clipMatrix = new SimpleModeLayer("clipMatrix") {
    case class PressedAt(var value: Instant)
    val clip: Clip = ext.host.createLauncherCursorClip(0, 0)

    override val modeBindings: Seq[Binding[_, _, _]] = j.matrix.indices.flatMap { col =>
      val track = trackBank.getItemAt(col)
      track.clipLauncherSlotBank().setIndication(true)
      track.isQueuedForStop.markInterested()

      val clips = track.clipLauncherSlotBank()

      val pressedAt: mutable.Seq[PressedAt] = mutable.ArraySeq.fill(8)(PressedAt(Instant.now()))

      EIGHT.flatMap { row =>
        val btn  = j.matrix(row)(col)
        val clip = clips.getItemAt(row)
        clip.color().markInterested()
        clip.hasContent.markInterested()
        clip.isPlaying.markInterested()
        clip.isSelected.markInterested()
        clip.isPlaybackQueued.markInterested()
        clip.isStopQueued.markInterested()
        clips.exists().markInterested()

        Vector(
          SupColorStateB(btn.light, () => clipColor(track, clip), JamColorState.empty),
          EB(btn.st.press, s"clipPress $row:$col", () => handleClipPress(clip, clips, pressedAt(col))),
          EB(btn.st.release, s"clipRelease $row:$col", () => handleClipRelease(clip, clips, pressedAt(col))),
        )
      }
    } ++ Vector(
      // FIXME: last released combo button needs to be handled as combo, not single - fixed?
      EB(j.duplicate.st.release, "dup clips: clear source", () => {
        source = None
      }, BB(tracked = false, managed = false)),
      EB(j.ShiftDup, _.press, "dup clips: duplicate content", () => clip.duplicateContent)
    )


    // for duplication
    private var source: Option[ClipLauncherSlot] = None

    private def handleClipPress(clip: ClipLauncherSlot, clips: ClipLauncherSlotBank, pressedAt: PressedAt): Unit = {
      if (GlobalMode.Select.isOn) clip.select()
      else if (GlobalMode.Clear.isOn) clip.deleteObject()
           else if (GlobalMode.Duplicate.isOn) {
             if (source.isEmpty) source = Some(clip)
             else {
               source.foreach(s => if (s != clip) clip.replaceInsertionPoint().copySlotsOrScenes(s))
               source = None
             }
           }
                else {
                  pressedAt.value = Instant.now()
                }
    }

    private def handleClipRelease(clip: ClipLauncherSlot, clips: ClipLauncherSlotBank, pressedAt: PressedAt): Unit = {
      if (Instant.now().isAfter(pressedAt.value.plus(Duration.ofSeconds(1))))
        clip.select()
      else if (clip.isPlaying.get() && ext.transport.isPlaying.get()) clips.stop()
           else clip.launch()
    }

    private def clipColor(track: Track, clip: ClipLauncherSlot): JamColorState = {
      if (GlobalMode.Select.isOn && clip.isSelected.get())
        JamColorState(JamColorBase.WHITE, 3)
      else if (!GlobalMode.Select.isOn && source.contains(clip))
             JamColorState(JamColorBase.WHITE, if (j.Mod.blink) 3 else 1)
           else
             JamColorState(
               if (clip.hasContent.get())
                 JamColorState.toColorIndex(clip.color().get())
               else
                 JamColorBase.OFF,
               brightness = {
                 if (clip.isPlaying.get())
                   if (track.isQueuedForStop.get()) if (j.Mod.blink) 3 else -1
                   else 3
                 else if (clip.isPlaybackQueued.get()) if (j.Mod.blink) 0 else 3
                      else 0
               }
             )
    }
  }

}
