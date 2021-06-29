package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api._
import com.github.unthingable.{FilteredPage, MonsterJamExt, ShowHide, Util, jam}
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface._

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier
import scala.collection.mutable
import scala.util.Try

/*
Behavior definition for surface controls
 */

class Jam(implicit ext: MonsterJamExt) extends BindingDSL {

  implicit val j: JamSurface = new JamSurface()(ext)

  val EIGHT: Vector[Int] = (0 to 7).toVector

  object GlobalMode {
    // These only set their isOn flags and nothing else
    val Clear    : ModeButtonLayer = ModeButtonLayer("clear", j.clear, modeBindings = Seq.empty, GateMode.Gate)
    val Duplicate: ModeButtonLayer = ModeButtonLayer("duplicate", j.duplicate, modeBindings = Seq.empty, GateMode.Gate)
    val Select   : ModeButtonLayer = ModeButtonLayer("select", j.select, modeBindings = Seq.empty, GateMode.Gate)
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


  val auxLayer = new ModeCycleLayer("aux", j.aux, CycleMode.Select) with Util {
    override val subModes = EIGHT.map(idx =>
      new SliderBankMode[Send]("strips aux", trackBank.getItemAt(_).sendBank().getItemAt(idx), identity) {
        override val barMode: BarMode = BarMode.SINGLE
      })
  }

  val auxGate = new ModeButtonLayer("strip aux gate", j.aux,
    GateMode.Gate,
    silent = true
  ) {
    val bank = ext.host.createEffectTrackBank(8, 1)
    override val modeBindings: Seq[Binding[_, _, _]] =
      j.groupButtons.zipWithIndex.flatMap { case (btn, idx) =>
        val color = bank.getItemAt(idx).color()
        color.markInterested()

        Vector(
          HB(btn.pressedAction, s"aux select $idx", () => auxLayer.select(idx)),
          SupColorStateB(btn.light, () =>
            (if (auxLayer.selected.contains(idx))
               JamColorState(JAMColorBase.WHITE, 3)
             else
               JamColorState(color.get(), 0)),
            JamColorState.empty))
      }
  }

  val levelCycle = new ModeCycleLayer("level", j.level, CycleMode.Cycle) with Util {
    override val subModes = Vector(
      new SliderBankMode[Track]("strips volume", trackBank.getItemAt, _.volume()) {
        EIGHT.foreach { idx =>
          val track = trackBank.getItemAt(idx)
          track.trackType().markInterested()
          track.trackType().addValueObserver(_ => updateLimits(Some(idx)))
        }
        ext.preferences.limitLevel.markInterested()
        ext.preferences.limitLevel.addValueObserver(_ => updateLimits(None))

        val paramLimits: mutable.Seq[Double] = mutable.ArrayBuffer.fill(8)(1.0)

        override val barMode: BarMode = BarMode.DUAL

        proxies.forindex { case (track, idx) =>
          val strip: JamTouchStrip = j.stripBank.strips(idx)
          track.addVuMeterObserver(128, -1, true, v => if (isOn) strip.update(v))
        }

        override def activate(): Unit = {
          super.activate()
          j.stripBank.strips.foreach(_.update(0))
        }

        override def paramRange(idx: Int): (Double, Double) = (0.0, paramLimits(idx))

        def updateLimits(maybeIdx: Option[Int]): Unit = {
          val max      = 1.259921049894873
          val zero     = 1.0
          val minusTen = 0.6812920690579614

          maybeIdx match {
            case Some(idx) =>
              paramLimits.update(idx, ext.preferences.limitLevel.get() match {
                case "None"   => 1.0
                case "Smart"  =>
                  trackBank.getItemAt(idx).trackType().get() match {
                    case "Group" => zero / max
                    case _       => minusTen / max
                  }
                case "0 dB"   => zero / max
                case "-10 dB" => minusTen / max
                case _        => 1.0
              })
            case None => EIGHT.map(Some.apply).foreach(updateLimits)
          }
        }
      },
      new SliderBankMode[Track]("strips pan", trackBank.getItemAt, _.pan()) {
        override val barMode: BarMode = BarMode.PAN
      },
    )
  }

  val dpad = new SimpleModeLayer("dpad") {
    val actionMap = Vector(
      j.dpad.left -> trackBank.canScrollBackwards,
      j.dpad.right -> trackBank.canScrollForwards,
      j.dpad.up -> sceneBank.canScrollBackwards,
      j.dpad.down -> sceneBank.canScrollForwards
    )

    actionMap.foreach { case (b: JamOnOffButton, e: BooleanValue) =>
      e.markInterested()
    }

    def scroll(forward: Boolean, target: Scrollable): HardwareActionBindable = {
      ext.host.createAction(() =>
        (j.Modifiers.Shift.isPressed(), forward) match {
          case (false, true)  => target.scrollPageForwards()
          case (false, false) => target.scrollPageBackwards()
          case (true, true)   => target.scrollForwards()
          case (true, false)  => target.scrollBackwards()
        }, () => s"scroll_$forward")
    }

    override val modeBindings: Seq[Binding[_, _, _]] = Vector(
      HB(j.dpad.left.pressedAction, "page left", scroll(false, trackBank)),
      HB(j.dpad.right.pressedAction, "page right", scroll(true, trackBank)),
      HB(j.dpad.up.pressedAction, "page up", scroll(false, sceneBank)),
      HB(j.dpad.down.pressedAction, "page down", scroll(true, sceneBank)),
    ) ++ actionMap.map { case (b: JamOnOffButton, e: BooleanValue) =>
      SupBooleanB(b.light.isOn, e)
    }
  }

  {
    // meters
    masterTrack.addVuMeterObserver(128, 0, true, j.levelMeter.uL)
    masterTrack.addVuMeterObserver(128, 1, true, j.levelMeter.uR)
  }

  {
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

    val position = SimpleModeLayer("position",
      Vector(HB(j.encoder.turn, "position turn", ext.host.createRelativeHardwareControlStepTarget(
              ext.transport.fastForwardAction(),
              ext.transport.rewindAction()))))

    val tempoLayer = ModeButtonLayer("tempo",
      j.tempo,
      Vector(
        HB(j.encoder.turn, "tempo turn", ext.host.createRelativeHardwareControlStepTarget(
                  action("inc tempo", () => ext.transport.increaseTempo(1, 647)),
                  action("dec tempo", () => ext.transport.increaseTempo(-1, 647))))))

    val play = new SimpleModeLayer("play") {
      ext.transport.isPlaying.markInterested()

      val playPressAction: HardwareActionBindable = action(s"$name play pressed", () => {
        val isPlaying = ext.transport.isPlaying
        val t         = ext.transport
        (isPlaying.get(), j.Modifiers.Shift.isPressed()) match {
          // just play
          case (true, false) => t.play()
          // restart (and stop)
          case (true, true) => t.restart()
          // resume
          case (false, false) => t.continuePlayback()
          case (false, true)  => restart(false)
        }
      })

      def restart(go: Boolean): Unit = {
        val h = ext.host
        ext.transport.stop()
        h.scheduleTask(() => {
          ext.transport.stop()
          h.scheduleTask(() => {
            ext.transport.stop()
            if (go) h.scheduleTask(() => ext.transport.play(), 10)
          }, 10)
        }, 10)
      }

      override val modeBindings = Vector(
        HB(j.play.pressedAction, "play pressed", playPressAction, tracked = false),
        SupBooleanB(j.play.light.isOn, ext.transport.isPlaying),
      )
    }

    val shiftTransport = new ModeButtonLayer("shiftTransport", j.Modifiers.Shift, GateMode.Gate) {
      val loop   : SettableBooleanValue = ext.transport.isArrangerLoopEnabled
      val overdub: SettableBooleanValue = ext.transport.isClipLauncherOverdubEnabled
      val metro                         = ext.transport.isMetronomeEnabled
      loop.markInterested()
      overdub.markInterested()
      metro.markInterested()

      def b(button: OnOffButton, name: String, param: SettableBooleanValue) = Vector(
        HB(button.pressedAction, s"shiftTransport $name pressed", () => param.toggle()),
        SupBooleanB(button.light.isOn, param)
      )

      override val modeBindings = Vector(
        b(j.right, "loop", loop),
        b(j.record, "record", overdub),
        b(j.left, "metro", metro),
      ).flatten ++ Vector(
        HB(j.tempo.pressedAction, "tap tempo", ext.transport.tapTempoAction())
      )
    }

    val globalQuant = new ModeButtonLayer("globalQuant",
      modeButton = j.grid,
      gateMode = GateMode.Gate
    ) {
      val quant = ext.transport.defaultLaunchQuantization()
      quant.markInterested()
      val enumValues = Vector("8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16")
      override val modeBindings = j.sceneButtons.indices.flatMap { idx =>
        val sceneButton = j.sceneButtons(idx)

        Vector(
          SupColorB(sceneButton.light, () =>
            if (quant.get() == enumValues(idx)) Color.whiteColor() else Color.blackColor()),
          HB(sceneButton.pressedAction, "global quant grid", action(s"grid $idx", () => {
                      if (quant.get == enumValues(idx))
                        quant.set("none")
                      else
                        quant.set(enumValues(idx))
                    })))
      }
    }

    def buttonGroupChannelMode(
      name: String,
      modeButton: JamOnOffButton,
      group: Seq[JamRgbButton],
      prop: Channel => SettableBooleanValue,
      color: Int // Jam's color index
    ): ModeButtonLayer = ModeButtonLayer(name, modeButton, group.indices.flatMap { idx =>
      val track       = trackBank.getItemAt(idx)
      val propValue   = prop(track)
      val existsValue = track.exists()
      propValue.markInterested()
      existsValue.markInterested()
      val gButton = group(idx)

      Vector(
        HB(gButton.pressedAction, s"group $idx pressed: $name", () => propValue.toggle()),
        SupColorStateB(gButton.light, () => {
          (existsValue.get(), propValue.get()) match {
            case (false, _) => JamColorState.empty
            case (_, false) => JamColorState(color, 0)
            case (_, true)  => JamColorState(color, 3)
          }
        }, JamColorState.empty)
      )
    }
    )

    val solo = buttonGroupChannelMode("solo", j.solo, j.groupButtons, _.solo(), JAMColorBase.YELLOW)
    val mute = buttonGroupChannelMode("mute", j.mute, j.groupButtons, _.mute(), JAMColorBase.ORANGE)

    val trackGroup = new SimpleModeLayer("trackGroup") {
      val foldToggleTop: Action = ext.application.getAction("toggle_top_level_track_groups_expanded")
      val foldToggleAll: Action = ext.application.getAction("toggle_all_track_groups_expanded")
      ext.cursorTrack.position().markInterested()

      override val modeBindings: Seq[Binding[_, _, _]] = j.groupButtons.indices flatMap { idx =>
        val btn = j.groupButtons(idx)
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

      override val modeBindings: Seq[Binding[_, _, _]] = Vector(
        SupBooleanB(j.dpad.up.light.isOn, () => !isAtTop.get()),
        SupBooleanB(j.dpad.down.light.isOn, track.isGroup),
        SupBooleanB(j.dpad.left.light.isOn, () => false),
        SupBooleanB(j.dpad.right.light.isOn, () => false),
        HB(j.dpad.up.pressedAction, "exit group", () => ext.application.navigateToParentTrackGroup()),
        HB(j.dpad.down.pressedAction, "enter group", () => ext.application.navigateIntoTrackGroup(track)),
        HB(j.dpad.left.pressedAction, "ignore left", () => ()),
        HB(j.dpad.right.pressedAction, "ignore right", () => ()),
        SupBooleanB(j.solo.light.isOn, track.solo()),
        SupBooleanB(j.mute.light.isOn, track.mute()),
        HB(j.solo.pressedAction, "track direct solo", track.solo().toggleAction()),
        HB(j.mute.pressedAction, "track direct mute", track.mute().toggleAction()),
      )
    }

    /**
     * Default clip matrix with clip launchers
     */
    val clipMatrix = new SimpleModeLayer("clipMatrix") {
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
        //ext.host.println("boom")
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
                 clip.color().get(),
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

    var superScene = new ModeButtonLayer("superScene", j.song, GateMode.Toggle) with Util {
      val maxTracks = 64 // can be up to 256 before serialization needs to be rethought
      val maxScenes = 64
      val bufferSize = maxTracks * maxScenes * 4
      var pageIndex = 0
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
        fromSettings(sceneStore.get()).forindex {case (m, idx) => superScenes.update(idx, m)}
      })

      (0 until maxTracks).foreach {t =>
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

      def recall(scene: Int): Unit = {
        (0 until maxTracks).foreach { track =>
          val id = TrackId(track)
          superScenes(scene).get(id) match {
            case Some(clip) =>
              // If a scene has a clip for a track id, attempt to launch it
              tracker.getItemAt(id).foreach(_.clipLauncherSlotBank().launch(clip))
            case None        =>
              // Otherwise attempt to stop
              tracker.getItemAt(id).foreach(_.stop())
          }
        }
        lastScene = Some(scene)
      }

      def pressed(scene: Int): Unit = {
        if (GlobalMode.Clear.isOn) superScenes.update(scene, Map.empty)
        else if (superScenes(scene).isEmpty) {
          superScenes.update(scene, scan().map(ct => ct.trackId -> ct.clip).toMap)

          val ser = serialize(maxTracks, maxScenes)(superScenes)
          //ext.host.println(ser)
          sceneStore.set(ser)
        } else
            recall(scene)
      }

      def serialize(rows: Int, cols: Int)(o: Iterable[Map[TrackId, Int]]): String =
        o.take(rows).map { row =>
          (0 until cols).map { idx => // danger zone: we depend on TrackId implementation being and int-based byte
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

    /**
     * Shift matrix row
     */
    val shiftMatrix = new ModeButtonLayer("shiftMatrix", j.Modifiers.Shift, GateMode.Gate) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] =
        (Vector(
          (JAMColorBase.RED, () => ext.application.undo()),
          (JAMColorBase.GREEN, () => ext.application.redo()),
          (JAMColorBase.LIME, () => clip.quantize(1.0)),
          (JAMColorBase.LIME, () => clip.quantize(0.5)),
          (JAMColorBase.MAGENTA, () => clip.transpose(-1)),
          (JAMColorBase.MAGENTA, () => clip.transpose(1)),
          (JAMColorBase.FUCHSIA, () => clip.transpose(-12)),
          (JAMColorBase.FUCHSIA, () => clip.transpose(12)),
        ).zipWithIndex.flatMap { case ((color, action), idx) =>
          val button = j.matrix(0)(idx)
          Vector(HB(button.pressedAction, s"shift-$idx matrix pressed", action)) ++ (
            if (ext.preferences.shiftRow.get())
              Vector(SupColorStateB(
                button.light, () => JamColorState(
                  color,
                  brightness = if (button.isPressed()) 2 else 0),
                JamColorState.empty))
            else Vector.empty
            )
        }
         ++ (if (ext.preferences.shiftRow.get()) Vector(
          SupColorStateB(j.matrix(1)(0).light, () =>
            if (ShowHide.withName(ext.docPrefs.hideDisabled.get()) == ShowHide.Hide)
              JamColorState(JAMColorBase.RED, 0)
            else JamColorState(JAMColorBase.YELLOW, 0)
          )) else Vector.empty)
         ++ Vector(
          HB(j.matrix(1)(0).pressedAction, "toggle hide disabled", () => {
            if (ShowHide.withName(ext.docPrefs.hideDisabled.get()) == ShowHide.Hide)
              ext.docPrefs.hideDisabled.set(ShowHide.Show.toString)
            else ext.docPrefs.hideDisabled.set(ShowHide.Hide.toString)
          })
        ))
    }

    val shiftPages = new ModeCycleLayer("shiftMatrix", j.Modifiers.Shift, CycleMode.GateSelect) {
      trackBank.itemCount().markInterested()
      trackBank.scrollPosition().markInterested()
      sceneBank.itemCount().markInterested()
      sceneBank.scrollPosition().markInterested()
      ext.preferences.shiftGroup.markInterested()

      def bankB(b: Bank[_], btn: Int => JamRgbButton, idx: Int) = Vector(
        SupColorStateB(btn(idx).light, () =>
          if (b.itemCount().get() > idx * 8)
            if (((idx * 8) - 7 until (idx * 8) + 7) contains b.scrollPosition().get())
              JamColorState(JAMColorBase.WHITE, 2)
            else
              JamColorState(JAMColorBase.WARM_YELLOW, 0)
          else JamColorState.empty
          , JamColorState.empty),
        HB(btn(idx).pressedAction, "shift-scroll page $idx", () => b.scrollPosition().set(idx * 8))
      )

      val trackPages: Vector[Binding[_, _, _]] =
        if (ext.preferences.shiftGroup.get())
          EIGHT.flatMap(bankB(trackBank, j.groupButtons, _))
        else
          Vector.empty

      override val subModes = Vector(
        new SimpleModeLayer("scene pages") with IntActivatedLayer {
          override val modeBindings: Vector[Binding[_, _, _]] =
            EIGHT.flatMap(bankB(sceneBank, j.sceneButtons, _)) ++ trackPages
        },
        new SimpleModeLayer("superscene pages") with IntActivatedLayer {
          override val modeBindings: Vector[Binding[_, _, _]] = {
            trackPages ++
            EIGHT.flatMap(idx => bankB(sceneBank, j.matrix(7), idx) ++ Vector(
              SupColorStateB(j.sceneButtons(idx).light, () =>
                if (idx == superScene.pageIndex)
                  JamColorState(JAMColorBase.WHITE, 3)
                else if (superScene.lastScene.exists(i => EIGHT.map(_ + (idx * 8)).contains(i)))
                  JamColorState(JAMColorBase.LIME, 0)
                else if (superScene.page(idx).exists(_.nonEmpty))
                  JamColorState(JAMColorBase.ORANGE, 0)
                else JamColorState.empty
              , JamColorState.empty),
              HB(j.sceneButtons(idx).pressedAction, "super scene page $idx", () => superScene.pageIndex = idx)
            ))
          }
        }
      )

      override def activate(): Unit = {
        selected = if (superScene.isOn) Some(1) else Some(0)
        super.activate()
      }
    }

    val globalShift = new ModeButtonLayer("globalShift", j.Modifiers.Shift, GateMode.Gate) {
      val clip: Clip = ext.host.createLauncherCursorClip(8, 128)
      override val modeBindings: Seq[Binding[_, _, _]] = Vector(
        HB(j.duplicate.pressedAction, "shift dup clip content", () => clip.duplicateContent())
      )
    }

    // devices!
    val deviceLayer = new ModeCycleLayer("device", j.control, CycleMode.Select) {
      val touchFX                      = "MonsterFX"
      val device: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()
      val page  : FilteredPage         = FilteredPage(
        device.createCursorRemoteControlsPage(8),
        _ != touchFX)

      device.hasNext.markInterested()
      device.hasPrevious.markInterested()
      page.c.pageNames().markInterested()
      page.c.selectedPageIndex().markInterested()

      val secondCursor: CursorRemoteControlsPage         = device.createCursorRemoteControlsPage(touchFX, 8, "")
      var touchPage   : Option[CursorRemoteControlsPage] = None
      secondCursor.pageNames().markInterested()
      secondCursor.selectedPageIndex().markInterested()
      secondCursor.pageNames().addValueObserver(names => touchPage = names
        .zipWithIndex.find(_._1 == touchFX).map { case (_, idx) =>
        secondCursor.selectedPageIndex().set(idx)
        secondCursor
      })

      val userBank: UserControlBank = ext.host.createUserControls(8)

      override val subModes: Seq[SubModeLayer] = Vector(
        new SliderBankMode[RemoteControl]("strips remote", page.c.getParameter, identity) {
          override val barMode: BarMode = BarMode.SINGLE

          j.stripBank.strips.forindex { case (strip, idx) =>
            strip.button.isPressed.markInterested()
            strip.button.isPressed.addValueObserver(v => if (isOn) touchPage.foreach(tp =>
              if (idx < tp.getParameterCount) tp.getParameter(idx).value().set(if (v) 1 else 0)))
          }

          override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
            SupBooleanB(j.left.light.isOn, m(() => device.hasPrevious.get(), page.hasPrevious)),
            SupBooleanB(j.right.light.isOn, m(() => device.hasNext.get(), page.hasNext)),
            HB(j.left.pressedAction, "scroll left", m(() => device.selectPrevious(), page.selectPrevious)),
            HB(j.right.pressedAction, "scroll right", m(() => device.selectNext(), page.selectNext)),
          )
        },
        new SliderBankMode[Parameter]("strips user bank", userBank.getControl, identity) {
          override val barMode: BarMode = BarMode.SINGLE

          override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
            SupBooleanB(j.macroButton.light.isOn, () => true)
          )
        },
      )

      def m(default: () => Boolean, modePressed: () => Boolean): BooleanSupplier =
        () => if (modeButton.isPressed()) modePressed() else default()

      def m(default: () => Unit, modePressed: () => Unit): () => Unit =
        () => if (modeButton.isPressed()) modePressed() else default()

      override def activate(): Unit = {
        val idx = page.c.selectedPageIndex().get()
        val pageNames = page.c.pageNames().get()
        if (idx >= 0 && idx < pageNames.length && pageNames(idx) == touchFX)
          if (page.hasPrevious())
            page.selectPrevious()
          else
            page.selectNext()
        super.activate()
      }
    }

    val deviceSelector = new ModeCycleLayer("deviceSelector", j.control, CycleMode.GateSelect, silent = true) {
      val cursorDevice: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()

      override val subModes: Seq[ModeLayer with IntActivatedLayer] = Vector(
        // all device matrix
        new SimpleModeLayer("matrixSelector") with IntActivatedLayer {
          val deviceBanks: Seq[DeviceBank] = EIGHT.map(trackBank.getItemAt).map(_.createDeviceBank(8))
          deviceBanks.foreach { bank =>
            bank.canScrollForwards.markInterested()
            bank.canScrollBackwards.markInterested()
          }

          def selectDevice(trackIdx: Int, device: Device): Unit = {
            if (device.exists().get()) {
              ext.cursorTrack.selectChannel(trackBank.getItemAt(trackIdx))
              //cursorDevice.selectDevice(device)
              device.selectInEditor()
              //deviceBanks(trackIdx).scrollIntoView(device.position().get())
            }
          }

          def deviceColor(device: Device): Int = (device.isPlugin.get(), device.deviceType().get()) match {
            case (false, "audio-effect") => JAMColorBase.ORANGE
            case (false, "instrument")   => JAMColorBase.WARM_YELLOW
            case (false, "note-effect")  => JAMColorBase.CYAN
            case (true, "audio-effect")  => JAMColorBase.MAGENTA
            case (true, "instrument")    => JAMColorBase.LIME
            case (true, "note-effect")   => JAMColorBase.PLUM
            case (_, s)                  =>
              ext.host.println(s"unknown device $s")
              JAMColorBase.RED
          }

          override val modeBindings: Seq[Binding[_, _, _]] = deviceBanks.zipWithIndex.flatMap { case (bank, col) =>
            EIGHT.flatMap { row =>
              val mButton = j.matrix(row)(col)
              val device = bank.getItemAt(row)
              device.exists().markInterested()
              device.position().markInterested()
              device.deviceType().markInterested()
              device.isPlugin.markInterested()
              val isSelected = device.createEqualsValue(cursorDevice)
              isSelected.markInterested()

              Vector(
                HB(mButton.pressedAction, s"select device $col:$row", () => selectDevice(col, device)),
                HB(mButton.releasedAction, s"noop $col:$row", () => ()),
                SupColorStateB(mButton.light,
                  () => if (device.exists().get())
                          JamColorState(deviceColor(device), if (isSelected.get()) 3 else 0)
                        else JamColorState.empty,
                  JamColorState.empty),
              )
            }
          } ++ Vector(
            SupBooleanB(j.dpad.up.light.isOn, () => deviceBanks.exists(_.canScrollBackwards.get())),
            SupBooleanB(j.dpad.down.light.isOn, () => deviceBanks.exists(_.canScrollForwards.get())),
            HB(j.dpad.up.pressedAction, "device bank up", () => deviceBanks.foreach(_.scrollPageBackwards())),
            HB(j.dpad.down.pressedAction, "device bank down", () => deviceBanks.foreach(_.scrollPageForwards())),
          )
        },
        // device navigator - maybe todo,
        // noop mode (disable device selector)
        new SimpleModeLayer("noopSelector") with IntActivatedLayer {
          override val modeBindings: Seq[Binding[_, _, _]] = Vector.empty
        },

      )
      override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
        HB(j.select.pressedAction, "cycle device selectors", () => cycle()),
        HB(j.macroButton.pressedAction, "control userbank cycle", () => deviceLayer.cycle()),
      )
    }

    val stripGroup = Exclusive(levelCycle, auxLayer, deviceLayer)

    val macroLayer = new ModeButtonLayer("macroLayer", j.macroButton, GateMode.Gate, silent = true) {
      var bumpedStrip: Option[IntActivatedLayer] = None
      var bumpedSubMode: Option[Int] = None

      override def activate(): Unit = {
        super.activate()
        // dirty hack to show user controls
        bumpedStrip = stripGroup.layers.find(_.isOn).collect { case x: IntActivatedLayer => x }.filter(_ != deviceLayer)
        if (!deviceLayer.selected.contains(1)) {
          bumpedSubMode = deviceLayer.selected
          deviceLayer.select(1)
        }
        if (!deviceLayer.isOn) deviceLayer.activateAction.invoke()
      }

      override def deactivate(): Unit = {
        bumpedStrip.foreach(_.activateAction.invoke())
        bumpedSubMode.foreach(deviceLayer.select)
        bumpedStrip = None
        bumpedSubMode = None
        super.deactivate()
      }

      override val modeBindings: Seq[Binding[_, _, _]] =
        (0 until superBank.getCapacityOfBank).flatMap { superIdx =>
          val track = superBank.getItemAt(superIdx)
          val row = superIdx / 8
          val col = superIdx % 8
          val btn = j.matrix(row)(col)
          val isSelected = ext.cursorTrack.createEqualsValue(track)
          isSelected.markInterested()

          Vector(
            SupColorStateB(btn.light, () =>
              if (isSelected.get())
                JamColorState(JAMColorBase.WHITE, 3)
              else
                JamColorState(track.color().get(), 0)),
            HB(btn.pressedAction, "direct select track", () => ext.cursorTrack.selectChannel(track)),
            HB(btn.releasedAction, "direct select release", () => ()),
          )
        }
    }

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
      trackGroup -> Exclusive(EIGHT.map(trackGate):_*),
      bottom -> Coexist(sceneLayer, superScene),
      bottom -> Coexist(unmanaged),
    )
  }
}
