package com.github.unthingable.jam

import com.bitwig.extension.controller.api._
import com.github.unthingable.{MonsterJamExt, ShowHide, Util}
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface._
import com.github.unthingable.jam.layer._

import scala.collection.mutable
import scala.util.Try

/*
Behavior definition for surface controls
 */

/*
bugsies
- superscene active selector does not clear
- same, between projects
 */

class Jam(implicit val ext: MonsterJamExt)
  extends BindingDSL
  with Aux with TransportL with Level with Dpad with TrackL
  with ClipMatrix with Shift with Control with MacroL {

  implicit val j: JamSurface = new JamSurface()

  val EIGHT: Vector[Int] = (0 to 7).toVector

  object GlobalMode {
    // These only set their isOn flags and nothing else
    val Clear    : ModeButtonLayer = ModeButtonLayer("CLEAR", j.clear, modeBindings = Seq.empty, GateMode.Gate)
    val Duplicate: ModeButtonLayer = ModeButtonLayer("DUPLICATE", j.duplicate, modeBindings = Seq.empty, GateMode.Gate)
    val Select   : ModeButtonLayer = ModeButtonLayer("SELECT", j.select, modeBindings = Seq.empty, GateMode.Gate)
  }

  val trackBank = ext.trackBank
  trackBank.followCursorTrack(ext.cursorTrack)

  val superBank: TrackBank = ext.host.createMainTrackBank(64, 8, 64)
  //superBank.followCursorTrack(ext.cursorTrack)
  superBank.itemCount().markInterested()
  superBank.scrollPosition().markInterested()

  ext.preferences.smartTracker.markInterested()
  implicit val tracker: TrackTracker = {
    if (ext.preferences.smartTracker.get())
      new SmartTracker(superBank)
    else
      new DumbTracker(superBank)
  }

  val sceneBank  : SceneBank   = trackBank.sceneBank()
  val masterTrack: MasterTrack = ext.host.createMasterTrack(8)

  sceneBank.canScrollForwards.markInterested()
  sceneBank.canScrollBackwards.markInterested()
  sceneBank.itemCount().markInterested()

  trackBank.cursorIndex().markInterested()

  ext.docPrefs.hideDisabled.markInterested()
  ext.docPrefs.hideDisabled.addValueObserver { v =>
    val skip = (ShowHide.withName(v) != ShowHide.Show)
    trackBank.setSkipDisabledItems(skip)
    superBank.setSkipDisabledItems(skip)
  }
  //sceneBank.setIndication(true)

  {
    // meters
    masterTrack.addVuMeterObserver(128, 0, true, j.levelMeter.uL)
    masterTrack.addVuMeterObserver(128, 1, true, j.levelMeter.uR)
  }

  //val sceneLayer = new ModeCycleLayer("sceneCycle", j.song, CycleMode.Cycle, silent = true) {
  //  override val subModes: Seq[ModeLayer with IntActivatedLayer] = Vector(
  //
  //  )
  //
  //  override def modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
  //    SupBooleanB(j.song.light.isOn, () => selected.contains(1)),
  //  )
  //}

    // wire scene buttons
    val sceneLayer = new SimpleModeLayer("scene") {
      override val modeBindings: Seq[Binding[_, _, _]] = j.sceneButtons.indices.flatMap { i =>
        val btn  : JamRgbButton = j.sceneButtons(i)
        val scene: Scene        = sceneBank.getScene(i)
        scene.color.markInterested()
        scene.exists.markInterested()

        Vector(
          SupColorB(btn.light, scene.color()),
          HB(btn.pressedAction, s"scene $i press", () => handlePress(scene)))
      }

      private def handlePress(scene: Scene): Unit = {
        if (GlobalMode.Clear.isOn) scene.deleteObject()
        else if (GlobalMode.Duplicate.isOn) scene.nextSceneInsertionPoint().copySlotsOrScenes(scene)
             else scene.launch()
      }
    }

    var superScene = new ModeButtonLayer("superScene", j.song, GateMode.Toggle) with Util {
      val maxTracks              = 64 // can be up to 256 before serialization needs to be rethought
      val maxScenes              = 64
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



    val stripGroup = Exclusive(levelCycle, auxLayer, controlLayer)



    // Final assembly of all mode layers
    val top       = Coexist(SimpleModeLayer("-^-", modeBindings = Vector.empty))
    val bottom    = SimpleModeLayer("_|_", modeBindings = Vector.empty)
    val unmanaged = SimpleModeLayer("_x_", modeBindings = Vector.empty)

    new ModeDGraph(
      init = Vector(levelCycle, sceneLayer),
      dpad -> top,
      play -> top,
      position -> Coexist(tempoLayer),
      sceneLayer -> top,
      bottom -> Coexist(globalQuant, shiftTransport, shiftMatrix, globalShift, shiftPages),
      bottom -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
      trackGroup -> Exclusive(solo, mute),
      clipMatrix -> top,
      bottom -> stripGroup,
      bottom -> Coexist(auxGate, deviceSelector, macroLayer),
      trackGroup -> Exclusive(EIGHT.map(trackGate): _*),
      bottom -> Coexist(sceneLayer, superScene),
      bottom -> Coexist(unmanaged),
    )
}
