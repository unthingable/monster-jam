package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.BooleanValue
import com.bitwig.extension.controller.api.MasterTrack
import com.bitwig.extension.controller.api.Track
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.JamColorState

import java.time.Instant
import java.util.function.BooleanSupplier

trait TrackL:
  this: Jam =>
  /*
    Workaround for weird TrackBank.scrollBy() behavior - if next page is empty it won't scroll right,
    instead it will always jump to (last track index - bank size) [117008]
   */
  def scrollBy(n: Int): Unit =
    trackBank.scrollPosition().set(trackBank.scrollPosition().get() + n)

  lazy val trackGroup = new SimpleModeLayer("trackGroup"):
    ext.cursorTrack.position().markInterested()
    var lastPress: Option[Int] = None

    override val modeBindings: Seq[Binding[?, ?, ?]] = j.groupButtons.indices flatMap { idx =>
      val btn       = j.groupButtons(idx)
      var pressedOn = Instant.now()

      val track        = trackBank.getItemAt(idx)
      val color        = track.color()
      val cursorIndex  = trackBank.cursorIndex()
      val playingNotes = track.playingNotes()

      color.markInterested()
      playingNotes.markInterested()
      cursorIndex.markInterested()
      track.exists().markInterested()
      track.isGroup.markInterested()
      track.position().markInterested()
      track.trackType().markInterested()

      def handlePress(): Unit =
        val now = Instant.now()
        if lastPress.exists(_ != idx) then lastPress.foreach(i => scrollBy(i - idx))
        else if track.isGroup.get && now.isBefore(pressedOn.plusMillis(400)) then

          // val trackId  = tracker.trackId(track)
          // val callback = () => {
          //  for {
          //    id <- trackId
          //    pos <- tracker.positionForId(id)
          //  } yield {
          //    Util.println(s"hunting track $id at $pos")
          //    trackBank.scrollPosition().set(pos - idx)
          //  }
          //  ()
          // }
          //
          // tracker.addRescanCallback(callback)

          track.isGroupExpanded.toggle()
          // foldToggleTop.invoke()
        else if GlobalMode.Clear.isOn then track.deleteObject()
        else if GlobalMode.Duplicate.isOn then track.duplicate()
        else track.selectInMixer()
        end if
        pressedOn = now
        lastPress = Some(idx)
      end handlePress

      def handleRelease(): Unit =
        if lastPress.contains(idx) then lastPress = None

      Vector(
        SupColorStateB(
          btn.light,
          () =>
            JamColorState(
              color.get(),
              brightness = (playingNotes.get().length > 0, cursorIndex.get() == idx) match
                case (_, true)  => 3
                case (true, _)  => 2
                case (false, _) => 0
            ),
          JamColorState.empty
        ),
        // HB(btn.button.press(), () => trackBank.cursorIndex().set(idx))
        EB(btn.st.press, s"group $idx pressed: select in mixer", () => handlePress()),
        EB(btn.st.release, s"group $idx released", () => handleRelease())
      )
    }

  def trackGate(idx: Int) =
    new ModeButtonLayer(s"track gate $idx", j.groupButtons(idx), GateMode.Gate, silent = true):
      import com.github.unthingable.jam.surface.KeyMaster.*
      val track = trackBank.getItemAt(idx)
      val isAtTop = ext.host.getProject.getRootTrackGroup
        .createEqualsValue(ext.host.getProject.getShownTopLevelTrackGroup)

      track.isGroup.markInterested()
      isAtTop.markInterested()

      track.mute().markInterested()
      track.solo().markInterested()
      track.arm().markInterested()
      trackBank.scrollPosition().markInterested()

      override val modeBindings: Seq[Binding[?, ?, ?]] = Vector(
        SupBooleanB(j.dpad.up.light, () => !isAtTop.get() && j.Mod.blink3),
        SupBooleanB(j.dpad.down.light, () => track.isGroup.get() && j.Mod.blink3),
        SupBooleanB(j.dpad.left.light, () => true),
        SupBooleanB(j.dpad.right.light, () => true),
        EB(j.dpad.up.st.press, "exit group", () => ext.application.navigateToParentTrackGroup()),
        EB(
          j.dpad.down.st.press,
          "enter group",
          () => ext.application.navigateIntoTrackGroup(track)
        ),
        EB(j.dpad.left.st.press, "scroll left", () => scrollBy(idx - 7)),
        EB(j.dpad.right.st.press, "scroll right", () => scrollBy(idx)),
        SupBooleanB(j.solo.light, track.solo()),
        SupBooleanB(j.mute.light, track.mute()),
        SupBooleanB(j.record.light, track.arm()),
        // FIXME - fixed?
        EB(j.solo.st.press, "track direct solo", () => track.solo().toggle()),
        EB(j.mute.st.press, "track direct mute", () => track.mute().toggle),
        EB(j.record.st.press, "track direct arm", () => track.arm().toggle()),
      )

  lazy val masterButton = new SimpleModeLayer("master button"):
    val first: Track               = trackBank.getItemAt(0)
    val parent: Track              = first.createParentTrack(0, 0)
    val master: MasterTrack        = ext.host.createMasterTrack(0)
    val equalsMaster: BooleanValue = master.createEqualsValue(ext.cursorTrack)
    val equalsParent: BooleanValue = parent.createEqualsValue(ext.cursorTrack)

    parent.isGroup.markInterested()
    equalsMaster.markInterested()
    equalsParent.markInterested()

    parent.position().markInterested()

    def inGroup: Boolean = parent.isGroup.get() && parent.position().get() >= 0
    // if first track is not a group, parent will be "Project" track with index -1

    val equals: BooleanSupplier = () => if inGroup then equalsParent.get() else equalsMaster.get()

    val selectMaster: () => Unit = () =>
      val localMaster = if inGroup then parent else master
      ext.cursorTrack.selectChannel(localMaster)

    override val modeBindings: Seq[Binding[?, ?, ?]] = Vector(
      SupBooleanB(j.master.light, equals),
      EB(j.master.st.press, "focus on master", selectMaster),
    )
end TrackL
