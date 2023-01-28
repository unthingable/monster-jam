package com.github.unthingable.jam

import com.bitwig.`extension`.callback.IndexedBooleanValueChangedCallback
import com.bitwig.extension.controller.api.*
import com.github.unthingable.JamSettings.ShowHide
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.binding.BindingDSL
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.GlobalEvent
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.Graph.Coexist
import com.github.unthingable.framework.mode.Graph.Exclusive
import com.github.unthingable.framework.mode.Graph.ExclusiveOn
import com.github.unthingable.framework.mode.Graph.ModeDGraph
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.layer.*
import com.github.unthingable.jam.surface.*

class Jam(implicit val ext: MonsterJamExt)
    extends BindingDSL,
      Aux,
      TransportL,
      Level,
      Dpad,
      TrackL,
      ClipMatrix,
      Shift,
      Control,
      MacroL,
      SceneL,
      StepSequencer:

  implicit val j: JamSurface = new JamSurface()

  val EIGHT: Vector[Int] = (0 to 7).toVector

  object GlobalMode:
    // These only set their isOn flags and nothing else
    val Clear: ModeButtonLayer =
      ModeButtonLayer("CLEAR", j.clear, modeBindings = Seq.empty, GateMode.Gate)
    val Duplicate: ModeButtonLayer =
      ModeButtonLayer("DUPLICATE", j.duplicate, modeBindings = Seq.empty, GateMode.Gate)
    val Select: ModeButtonLayer =
      ModeButtonLayer("SELECT", j.select, modeBindings = Seq.empty, GateMode.Gate)

  lazy val trackBank = ext.trackBank
  trackBank.followCursorTrack(ext.cursorTrack)

  // val superBank: TrackBank = ext.host.createMasterTrack(256).createMainTrackBank(256, 8, 256, true)
  val superBank: TrackBank = ext.host.createTrackBank(64, 0, 64, true)
  superBank.itemCount().markInterested()
  // superBank.itemCount().addValueObserver(i => Util.println(s"superbank now $i"), 0)
  superBank.scrollPosition().markInterested()

  val selectedClipTrack = ext.cursorTrack // FIXME maybe abandon
  def selectedObserver(track: Int): IndexedBooleanValueChangedCallback =
    (idx: Int, selected: Boolean) =>
      if selected then ext.events.eval("selectObserver")(GlobalEvent.ClipSelected(track, idx))

  for i <- 0 until superBank.getCapacityOfBank() do
    superBank.getItemAt(i).clipLauncherSlotBank().addIsSelectedObserver(selectedObserver(i))

  given tracker: TrackTracker = UnsafeTracker(superBank)

  lazy val sceneBank: SceneBank     = trackBank.sceneBank()
  lazy val masterTrack: MasterTrack = ext.host.createMasterTrack(8)

  sceneBank.canScrollForwards.markInterested()
  sceneBank.canScrollBackwards.markInterested()
  sceneBank.itemCount().markInterested()

  trackBank.cursorIndex().markInterested()

  ext.docPrefs.hideDisabled.addValueObserver { v =>
    val skip = v != ShowHide.Show
    trackBank.setSkipDisabledItems(skip)
    superBank.setSkipDisabledItems(skip)
  }

  {
    // meters
    masterTrack.addVuMeterObserver(128, 0, true, j.levelMeter.uL)
    masterTrack.addVuMeterObserver(128, 1, true, j.levelMeter.uR)
  }

  val stripGroup = ExclusiveOn(levelCycle, auxLayer, controlLayer, stepSequencer.noteParam)

  // Final assembly of all mode layers
  val top       = Coexist(SimpleModeLayer("-^-", modeBindings = Vector.empty))
  val bottom    = SimpleModeLayer("_|_", modeBindings = Vector.empty)
  val unmanaged = SimpleModeLayer("_x_", modeBindings = Vector.empty)

  // experimental
  lazy val home: SimpleModeLayer = new SimpleModeLayer("home"):
    private var noRelease: Boolean = false
    override val modeBindings = Vector(
      EB(
        j.song.st.press,
        "song press (restore home)",
        () =>
          val (noClip :: noScene :: _) = Seq(clipMatrix, sceneCycle).map(graph.isOcculted): @unchecked
          Util.println(s"restore home clip/scene: $noClip $noScene")
          if noClip || noScene then
            graph.reactivate(sceneCycle)
            noRelease = true
          else
            sceneCycle.press()
            noRelease = false
          if noClip then graph.reactivate(clipMatrix)
        ,
        BB(
          managed = false,
          // exclusive = false
        )
      ),
    )

  val graph = new ModeDGraph(
    init = Vector(levelCycle, sceneCycle, clipMatrix, home, shiftTempo),
    dpad         -> top,
    play         -> top,
    position     -> Coexist(tempoLayer),
    sceneCycle   -> top,
    bottom       -> Coexist(globalQuant, shiftTransport, shiftMatrix, shiftPages),
    bottom       -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
    trackGroup   -> Exclusive(solo, mute, record),
    bottom       -> Coexist(clipMatrix, pageMatrix, stepSequencer, stepGate),
    bottom       -> stripGroup,
    bottom       -> Coexist(auxGate, deviceSelector, macroLayer),
    trackGroup   -> Exclusive(EIGHT.map(trackGate)*),
    masterButton -> top,
    bottom       -> Coexist(unmanaged),
  )

  // val newGraph = ModeCommander(
  //   clipMatrix,
  //   sceneLayer,
  //   levelCycle,
  // )
end Jam
