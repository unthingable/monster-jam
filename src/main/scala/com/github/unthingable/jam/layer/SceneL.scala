package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{Scene, SettableStringValue, Setting}
import com.github.unthingable.Util
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.{JamColorState, JamRgbButton}
import com.github.unthingable.jam.{Binding, CycleMode, HB, IntActivatedLayer, Jam, ModeCycleLayer, ModeLayer, SubModeLayer, SupBooleanB, SupColorB, SupColorStateB, TrackId}

import scala.collection.mutable
import scala.util.Try

trait SceneL { this: Jam =>
  lazy val sceneSub: SubModeLayer = new SubModeLayer("sceneSub") {
    override val modeBindings: Seq[Binding[_, _, _]] = j.sceneButtons.indices.flatMap { i =>
      val btn  : JamRgbButton = j.sceneButtons(i)
      val scene: Scene        = sceneBank.getScene(i)
      scene.color.markInterested()
      scene.exists.markInterested()

      Vector(
        SupColorStateB(btn.light, () => JamColorState(scene.color().get(), 1)),
        HB(btn.pressedAction, s"scene $i press", () => handlePress(scene)))
    }

    private def handlePress(scene: Scene): Unit = {
      if (GlobalMode.Clear.isOn) scene.deleteObject()
      else if (GlobalMode.Duplicate.isOn) scene.nextSceneInsertionPoint().copySlotsOrScenes(scene)
           else {
        superSceneSub.lastScene = None
        scene.launch()
      }
    }
  }

  lazy val superSceneSub = new SubModeLayer("superSceneSub") with Util {
    val maxTracks              = superBank.getSizeOfBank // can be up to 256 before serialization needs to be rethought
    val maxScenes              = superBank.sceneBank().getSizeOfBank
    val bufferSize             = maxTracks * maxScenes * 4
    var pageIndex              = 0
    var lastScene: Option[Int] = None

    val sceneStore: SettableStringValue = ext.document.getStringSetting("superScene", "MonsterJam", bufferSize, "")
    sceneStore.asInstanceOf[Setting].hide()

    lazy val superScenes: mutable.ArraySeq[Map[TrackId, Int]] = mutable.ArraySeq.from(fromSettings(sceneStore.get()))

    private def fromSettings(s: String): Iterable[Map[TrackId, Int]] =
      Try(deserialize(maxTracks, maxScenes)(s))
        .toEither
        .filterOrElse(_.nonEmpty, new Exception("Deserialized empty"))
        .left.map { e => ext.host.println(s"Failed to deserialize superscenes: ${e}"); e }
        .getOrElse(Vector.fill(maxTracks)(Map.empty))

    ext.application.projectName().markInterested()
    ext.application.projectName().addValueObserver(_ => {
      fromSettings(sceneStore.get()).forindex { case (m, idx) => superScenes.update(idx, m) }
    })

    (0 until maxTracks).foreach { t =>
      val clips = superBank.getItemAt(t).clipLauncherSlotBank()
      clips.itemCount().markInterested()
      (0 until maxScenes).foreach(c => clips.getItemAt(c).isPlaying.markInterested())
    }

    case class ClipTarget(trackId: TrackId, clip: Int)

    def scan(): Seq[ClipTarget] = (0 until maxTracks.min(superBank.itemCount().get)).flatMap { tIdx =>
      val scenes = superBank.getItemAt(tIdx).clipLauncherSlotBank()
      (0 until maxScenes.min(scenes.itemCount().get())).flatMap { sIdx =>
        val clip = scenes.getItemAt(sIdx)

        if (clip.isPlaying.get())
          tracker.idForPosition(tIdx).map(ClipTarget(_, sIdx))
        else
          None
      }
    }

    def recall(sceneIdx: Int): Unit = {
      (0 until maxTracks).map(TrackId).foreach { trackId =>
        superScenes(sceneIdx).get(trackId) match {
          case Some(clip) =>
            // If a scene has a clip for a track id, attempt to launch it
            tracker.getItemAt(trackId).foreach(_.clipLauncherSlotBank().launch(clip))
          case None       =>
            // Otherwise attempt to stop
            tracker.getItemAt(trackId).foreach(_.stop())
        }
      }
      lastScene = Some(sceneIdx)
    }

    def pressed(sceneIdx: Int): Unit = {
      if (GlobalMode.Clear.isOn) superScenes.update(sceneIdx, Map.empty)
      else if (superScenes(sceneIdx).isEmpty) {
        superScenes.update(sceneIdx, scan().map(ct => ct.trackId -> ct.clip).toMap)

        val ser = serialize(maxTracks, maxScenes)(superScenes)
        //ext.host.println(ser)
        sceneStore.set(ser)
      } else
          recall(sceneIdx)
    }

    def serialize(rows: Int, cols: Int)(o: Iterable[Map[TrackId, Int]]): String =
      o.take(rows).map { row =>
        (0 until cols).map { idx => // danger zone: we depend on TrackId implementation being an int-based byte
          f"${idx}%02x${row.get(TrackId(idx)).map(_ + 1).getOrElse(0)}%02x"
        }.mkString
      }.mkString

    def deserialize(rows: Int, cols: Int)(s: String): Iterable[Map[TrackId, Int]] = {
      assert(s.length == bufferSize, s"length mismatch: expected $bufferSize, got ${s.length}")
      assert(rows * cols * 4 == bufferSize, s"rows * cols (${rows * cols * 4}) does not match expected bufferSize $bufferSize")
      val ret = s.grouped(2).map(Integer.parseInt(_, 16))
        .grouped(2).map(s => s(0) -> (s(1) - 1))
        .grouped(cols).map(_.filter(_._2 != -1).map(t => (TrackId(t._1), t._2)).toMap)
        .toVector
      //assert(ret.forall(_.forall(x => x._1 < maxTracks && x._2 < maxScenes)), "index out of bounds")
      ret
    }

    def page(idx: Int): mutable.Seq[Map[TrackId, Int]] = superScenes.slice(idx * 8, (idx + 1) * 8)

    override val modeBindings: Seq[Binding[_, _, _]] = EIGHT.flatMap { idx =>
      def pageOffset = pageIndex * 8

      Vector(
        HB(j.sceneButtons(idx).pressedAction, s"super scene $idx pressed", () => pressed(pageOffset + idx)),
        SupColorStateB(j.sceneButtons(idx).light, () =>
          JamColorState(
            if (superScenes(pageOffset + idx).isEmpty)
              JAMColorBase.OFF
            else if (lastScene.contains(pageOffset + idx)) JAMColorBase.WHITE
                 else (((pageOffset + idx) % 16) + 1) * 4,
            if (lastScene.contains(pageOffset + idx)) 2 else 0), JamColorState.empty),
      )
    }
  }

  lazy val sceneLayer = new ModeCycleLayer("sceneCycle", j.song, CycleMode.Cycle, silent = true) {
    override val subModes: Seq[ModeLayer with IntActivatedLayer] = Vector(
      sceneSub,
      superSceneSub
    )

    override def modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
      SupBooleanB(j.song.light.isOn, () => selected.contains(1)),
    )
  }
}
