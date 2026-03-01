package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.Scene
import com.github.unthingable.Util
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.BindingBehavior
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.HB.BindingOps
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.mode.ModeCycleLayer
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.ChannelIdTracker
import com.github.unthingable.jam.LegacyTrackId
import com.github.unthingable.jam.TrackId
import com.github.unthingable.jam.TrackIdMigration
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamRgbButton

import java.time.Instant
import scala.collection.mutable

trait SceneL:
  this: Jam =>

  lazy val sceneSub: ModeLayer = new SimpleModeLayer("sceneSub"):
    override val modeBindings: Seq[Binding[?, ?, ?]] = j.sceneButtons.indices.flatMap { i =>
      val btn: JamRgbButton = j.sceneButtons(i)
      val scene: Scene      = sceneBank.getScene(i)
      scene.color.markInterested()
      scene.exists.markInterested()
      scene.clipCount().markInterested()

      Vector(
        SupColorStateB(
          btn.light,
          () =>
            val isPlaying = EIGHT.exists(col =>
              trackBank.getItemAt(col).clipLauncherSlotBank().getItemAt(i).isPlaying.get()
            )
            JamColorState(
              if scene.clipCount().get() > 0 then JamColorState.toColorIndex(scene.color().get())
              else JamColorBase.OFF,
              if isPlaying then 3 else 1
            )
        ),
        EB(btn.st.press, s"scene $i press", () => handlePress(scene, i))
      )
    }

    private def handlePress(scene: Scene, idx: Int): Unit =
      if GlobalMode.Clear.isOn then scene.deleteObject()
      else if GlobalMode.Duplicate.isOn then scene.nextSceneInsertionPoint().copySlotsOrScenes(scene)
      else
        val isPlaying = EIGHT.exists(col =>
          trackBank.getItemAt(col).clipLauncherSlotBank().getItemAt(idx).isPlaying.get()
        )
        if isPlaying then EIGHT.foreach(col => trackBank.getItemAt(col).stop())
        else
          superSceneSub.lastScene = None
          scene.launch()

  object superSceneSub extends SimpleModeLayer("superSceneSub") with Util:
    val maxTracks =
      superBank.getSizeOfBank // can be up to 256 before serialization needs to be rethought
    val maxScenes  = superBank.sceneBank().getSizeOfBank
    var pageIndex  = 0
    var lastScene: Option[Int] = None

    val superScenes: mutable.ArraySeq[Map[TrackId, Int]] =
      mutable.ArraySeq.from(fromSettings(sceneStore.get()))

    private def fromSettings(s: String): Iterable[Map[TrackId, Int]] =
      if s == null || s.isEmpty then return Vector.fill(maxTracks)(Map.empty)
      Util
        .deserialize[superScenes.type](s)
        .filterOrElse(_.nonEmpty, new Exception("Deserialized empty"))
        match
          case Right(data) => data
          case Left(e) =>
            // Try legacy format (Int-based TrackId with old serialVersionUID)
            type LegacyScenes = mutable.ArraySeq[Map[LegacyTrackId, Int]]
            TrackIdMigration.deserializeLegacy[LegacyScenes](s) match
              case Right(legacy) =>
                val mapping = TrackIdMigration.buildMapping(superBank, tracker.asInstanceOf[ChannelIdTracker])
                if mapping.nonEmpty then
                  val migrated = mutable.ArraySeq.from(legacy.map(_.flatMap {
                    case (LegacyTrackId(hash), clip) => mapping.get(hash).map(TrackId(_) -> clip)
                  }))
                  sceneStore.set(Util.serialize(migrated))
                  Util.println(s"Migrated superscenes in-place (${migrated.count(_.nonEmpty)} non-empty)")
                  migrated
                else
                  Util.println("Legacy superscenes found but track mapping not yet available")
                  Vector.fill(maxTracks)(Map.empty)
              case Left(_) =>
                Util.println(s"Failed to deserialize superscenes: $e")
                Vector.fill(maxTracks)(Map.empty)

    ext.application.projectName().markInterested()
    ext.application
      .projectName()
      .addValueObserver { _ =>
        fromSettings(sceneStore.get()).forindex { case (m, idx) => superScenes.update(idx, m) }
      }

    (0 until maxTracks).foreach { t =>
      val clips = superBank.getItemAt(t).clipLauncherSlotBank()
      clips.itemCount().markInterested()
      (0 until maxScenes).foreach(c => clips.getItemAt(c).isPlaying.markInterested())
    }

    case class ClipTarget(trackId: TrackId, clip: Int)

    def scan(): Seq[ClipTarget] = (0 until maxTracks.min(superBank.itemCount().get)).flatMap { tIdx =>
      val scenes                    = superBank.getItemAt(tIdx).clipLauncherSlotBank()
      val posMap: Map[Int, TrackId] = tracker.idMap.map(_.swap).toMap
      (0 until maxScenes.min(scenes.itemCount().get())).flatMap { sIdx =>
        val clip = scenes.getItemAt(sIdx)

        if clip.isPlaying.get() then posMap.get(tIdx).map(ClipTarget(_, sIdx))
        else None
      }
    }

    def recall(sceneIdx: Int): Unit =
      val posMap: Map[Int, TrackId] = tracker.idMap.map(_.swap).toMap
      (0 until maxTracks).foreach { idx =>
        val track = superBank.getItemAt(idx)
        posMap.get(idx).flatMap(superScenes(sceneIdx).get) match
          case Some(clip) =>
            // If a scene has a clip for a track id, attempt to launch it
            track.clipLauncherSlotBank().launch(clip)
          case None =>
            // Otherwise attempt to stop
            track.stop()
      }
      lastScene = Some(sceneIdx)

    def pressed(sceneIdx: Int): Unit =
      if GlobalMode.Clear.isOn then superScenes.update(sceneIdx, Map.empty)
      else if lastScene.contains(sceneIdx) then
        (0 until maxTracks).foreach(idx => superBank.getItemAt(idx).stop())
        lastScene = None
      else if superScenes(sceneIdx).isEmpty then
        superScenes.update(sceneIdx, scan().map(ct => ct.trackId -> ct.clip).toMap)
        val data = Util.serialize(superScenes)
        Util.println(s"saving superScenes: ${data.size} chars")
        sceneStore.set(data)
      else recall(sceneIdx)

    def page(idx: Int): mutable.Seq[Map[TrackId, Int]] = superScenes.slice(idx * 8, (idx + 1) * 8)

    override val modeBindings: Seq[Binding[?, ?, ?]] = EIGHT.flatMap { idx =>
      def pageOffset = pageIndex * 8

      Vector(
        EB(
          j.sceneButtons(idx).st.press,
          s"super scene $idx pressed",
          () => pressed(pageOffset + idx)
        ),
        SupColorStateB(
          j.sceneButtons(idx).light,
          () =>
            JamColorState(
              if superScenes(pageOffset + idx).isEmpty then JamColorBase.OFF
              else if lastScene.contains(pageOffset + idx) then JamColorBase.WHITE
              else (((pageOffset + idx) % 16) + 1) * 4,
              if lastScene.contains(pageOffset + idx) then 2 else 0
            ),
          JamColorState.empty
        ),
      )
    } :+ SupBooleanB(j.song.light, () => isOn, BindingBehavior.soft)
  end superSceneSub

  lazy val pageMatrix = new SimpleModeLayer("pageMatrix"):
    private var trackPos: Int = -1
    private var scenePos: Int = -1
    private var trackLen: Int = -1
    private var sceneLen: Int = -1

    trackBank.scrollPosition().addValueObserver(v => trackPos = v)
    sceneBank.scrollPosition().addValueObserver(v => scenePos = v)
    trackBank.itemCount().addValueObserver(v => trackLen = (v - 1) / 8)
    sceneBank.itemCount().addValueObserver(v => sceneLen = (v - 1) / 8)

    override val modeBindings: Seq[Binding[?, ?, ?]] =
      (for (row <- EIGHT; col <- EIGHT) yield
        val btn: JamRgbButton = j.matrix(row)(col)

        def hasContent = trackLen >= col && sceneLen >= row
        def ourPage = Seq(scenePos, scenePos + 7).map(_ / 8).contains(row) &&
          Seq(
            trackPos,
            trackPos + 7
          ).map(_ / 8).contains(col)

        Vector(
          SupColorStateB(
            btn.light,
            () =>
              if hasContent then
                if ourPage then JamColorState(JamColorBase.WHITE, 2)
                else JamColorState(JamColorBase.WARM_YELLOW, 0)
              else JamColorState.empty,
            JamColorState.empty
          ),
          EB(
            btn.st.press,
            "shift-scroll page $idx",
            () =>
              trackBank.scrollPosition().set(col * 8)
              sceneBank.scrollPosition().set(row * 8)
          )
        )
      ).flatten

  // a hybrid: both a cycle layer for scene buttons and a controller for page matrix
  object sceneCycle extends ModeCycleLayer("sceneCycle"):
    override val subModes: Vector[ModeLayer] = Vector(
      sceneSub,
      superSceneSub
    )

    private var pressedAt: Option[Instant] = None

    def press(): Unit =
      pressedAt = Some(Instant.now())
      ext.host.scheduleTask(
        () => if j.song.btn.isPressed().get then ext.events.eval("sceneL press")(pageMatrix.activateEvent*),
        80
      )

    def release(): Unit =
      if pageMatrix.isOn then ext.events.eval("sceneL release")(pageMatrix.deactivateEvent*)

      if pressedAt.exists(instant =>
          instant.plusMillis(400).isAfter(Instant.now()) || pageMatrix.modeBindings.hasOperatedAfter(instant)
        )
      then cycle()

      pressedAt = None

    override val modeBindings: Seq[Binding[?, ?, ?]] = Vector(
      SupBooleanB(j.song.light, () => superSceneSub.isOn),
      // taken over by experimental home mode
      // EB(j.song.st.press, "sceneCycle pressed", () => press()),
      EB(j.song.st.release, "sceneCycle released", () => if pressedAt.nonEmpty then release()),
    )
  end sceneCycle
end SceneL
