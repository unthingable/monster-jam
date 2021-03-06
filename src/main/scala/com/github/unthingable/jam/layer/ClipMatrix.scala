package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{ClipLauncherSlot, ClipLauncherSlotBank, Track}
import com.github.unthingable.jam.{Binding, BindingBehavior, HB, Jam, SimpleModeLayer, SupColorStateB}
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.JamColorState

import java.time.{Duration, Instant}
import scala.collection.mutable

trait ClipMatrix { this: Jam =>
  lazy val clipMatrix = new SimpleModeLayer("clipMatrix") {
    case class PressedAt(var value: Instant)

    override val modeBindings: Seq[Binding[_, _, _]] = j.matrix.indices.flatMap { col =>
      val track = trackBank.getItemAt(col)
      track.clipLauncherSlotBank().setIndication(true)
      track.isQueuedForStop.markInterested()

      val clips = track.clipLauncherSlotBank()

      val pressedAt: mutable.Seq[PressedAt] = mutable.ArraySeq.fill(8)(PressedAt(Instant.now()))

      (0 to 7).flatMap { row =>
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
          HB(btn.pressedAction, s"clipPress $row:$col", () => handleClipPress(clip, clips, pressedAt(col))),
          HB(btn.releasedAction, s"clipRelease $row:$col", () => handleClipRelease(clip, clips, pressedAt(col))),
        )
      }
    } :+ HB(GlobalMode.Duplicate.deactivateAction, "dup clips: clear source", () => {
      source = None
    }, tracked = false, behavior = BindingBehavior(managed = false))

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
      else if (clip.isPlaying.get()) clips.stop()
           else clip.launch()
    }

    private def clipColor(track: Track, clip: ClipLauncherSlot): JamColorState = {
      if (GlobalMode.Select.isOn && clip.isSelected.get())
        JamColorState(JAMColorBase.WHITE, 3)
      else if (!GlobalMode.Select.isOn && source.contains(clip))
             JamColorState(JAMColorBase.WHITE, if (j.Modifiers.blink) 3 else 1)
           else
             JamColorState(
               if (clip.hasContent.get())
                 JamColorState.toColorIndex(clip.color().get())
               else
                 JAMColorBase.OFF,
               brightness = {
                 if (clip.isPlaying.get())
                   if (track.isQueuedForStop.get()) if (j.Modifiers.blink) 3 else -1
                   else 3
                 else if (clip.isPlaybackQueued.get()) if (j.Modifiers.blink) 0 else 3
                      else 0
               }
             )
    }
  }

}
