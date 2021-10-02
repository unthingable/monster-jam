package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{Action, BooleanValue, MasterTrack, Track}
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.{Binding, GateMode, HB, Jam, ModeButtonLayer, SimpleModeLayer, SupBooleanB, SupColorStateB}

import java.time.Instant
import java.util.function.BooleanSupplier

trait TrackL { this: Jam =>
  lazy val trackGroup = new SimpleModeLayer("trackGroup") {
    val foldToggleTop: Action = ext.application.getAction("toggle_top_level_track_groups_expanded")
    val foldToggleAll: Action = ext.application.getAction("toggle_all_track_groups_expanded")
    ext.cursorTrack.position().markInterested()

    override val modeBindings: Seq[Binding[_, _, _]] = j.groupButtons.indices flatMap { idx =>
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

      def handlePress(): Unit = {
        val now = Instant.now()
        if (track.isGroup.get && now.isBefore(pressedOn.plusMillis(400))) {

          val trackId  = tracker.trackId(track)
          val callback = () => {
            for {
              id <- trackId
              pos <- tracker.positionForId(id)
            } yield {
              ext.host.println(s"hunting track $id at $pos")
              trackBank.scrollPosition().set(pos - idx)
            }
            ()
          }

          tracker.addRescanCallback(callback)

          foldToggleTop.invoke()

        } else if (GlobalMode.Clear.isOn) track.deleteObject()
               else if (GlobalMode.Duplicate.isOn) track.duplicate()
                    else track.selectInMixer()
        pressedOn = now
      }

      Vector(
        SupColorStateB(btn.light, () => JamColorState(
          color.get(),
          brightness = (playingNotes.get().length > 0, cursorIndex.get() == idx) match {
            case (_, true)  => 3
            case (true, _)  => 2
            case (false, _) => 0
          }
        ), JamColorState.empty),
        //HB(btn.button.pressedAction(), () => trackBank.cursorIndex().set(idx))
        HB(btn.pressedAction, s"group $idx pressed: select in mixer", () => handlePress())
      )
    }
  }

  def trackGate(idx: Int) = new ModeButtonLayer(s"track gate $idx", j.groupButtons(idx),
    GateMode.Gate,
    silent = true
  ) {
    val track   = trackBank.getItemAt(idx)
    val isAtTop = ext.host.getProject.getRootTrackGroup.createEqualsValue(ext.host.getProject.getShownTopLevelTrackGroup)

    track.isGroup.markInterested()
    isAtTop.markInterested()

    track.mute().markInterested()
    track.solo().markInterested()
    track.arm().markInterested()

    override val modeBindings: Seq[Binding[_, _, _]] = Vector(
      SupBooleanB(j.dpad.up.light.isOn, () => !isAtTop.get() && j.Modifiers.blink3),
      SupBooleanB(j.dpad.down.light.isOn, () => track.isGroup.get() && j.Modifiers.blink3),
      SupBooleanB(j.dpad.left.light.isOn, () => false),
      SupBooleanB(j.dpad.right.light.isOn, () => false),
      HB(j.dpad.up.pressedAction, "exit group", () => ext.application.navigateToParentTrackGroup()),
      HB(j.dpad.down.pressedAction, "enter group", () => ext.application.navigateIntoTrackGroup(track)),
      HB(j.dpad.left.pressedAction, "ignore left", () => ()),
      HB(j.dpad.right.pressedAction, "ignore right", () => ()),
      SupBooleanB(j.solo.light.isOn, track.solo()),
      SupBooleanB(j.mute.light.isOn, track.mute()),
      SupBooleanB(j.record.light.isOn, track.arm()),
      HB(j.solo.pressedAction, "track direct solo", track.solo().toggleAction()),
      HB(j.mute.pressedAction, "track direct mute", track.mute().toggleAction()),
      HB(j.record.pressedAction, "track direct arm", track.arm().toggleAction()),
    )
  }

  def masterButton = new SimpleModeLayer("master button") {
    val first : Track = trackBank.getItemAt(0)
    val parent: Track = first.createParentTrack(0,0)
    val master: MasterTrack = ext.host.createMasterTrack(0)
    val equalsMaster: BooleanValue = master.createEqualsValue(ext.cursorTrack)
    val equalsParent: BooleanValue = parent.createEqualsValue(ext.cursorTrack)

    parent.isGroup.markInterested()
    equalsMaster.markInterested()
    equalsParent.markInterested()

    val equals: BooleanSupplier = () => if (parent.isGroup.get()) equalsParent.get() else equalsMaster.get()

    val selectMaster: () => Unit = () => {
      val localMaster = if (parent.isGroup.get()) parent else master
      ext.cursorTrack.selectChannel(localMaster)
    }

    override def modeBindings: Seq[Binding[_, _, _]] = Vector(
      SupBooleanB(j.master.light.isOn, equals),
      HB(j.master.pressedAction, "focus on master", selectMaster),
    )
  }

}
