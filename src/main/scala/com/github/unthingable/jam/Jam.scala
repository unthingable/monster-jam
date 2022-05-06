package com.github.unthingable.jam

import com.bitwig.extension.controller.api._
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.JamSettings.ShowHide
import com.github.unthingable.framework.mode.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.framework.mode.{GateMode, ModeButtonLayer, ModeCommander, SimpleModeLayer}
import com.github.unthingable.framework.binding.BindingDSL
import com.github.unthingable.jam.surface._
import com.github.unthingable.jam.layer._

class Jam(implicit val ext: MonsterJamExt)
  extends BindingDSL
  with Aux with TransportL with Level with Dpad with TrackL
  with ClipMatrix with Shift with Control with MacroL with SceneL
  with StepSequencer {

  implicit val j: JamSurface = new JamSurface()

  val EIGHT: Vector[Int] = (0 to 7).toVector

  object GlobalMode {
    // These only set their isOn flags and nothing else
    val Clear    : ModeButtonLayer = ModeButtonLayer("CLEAR", j.clear, modeBindings = Seq.empty, GateMode.Gate)
    val Duplicate: ModeButtonLayer = ModeButtonLayer("DUPLICATE", j.duplicate, modeBindings = Seq.empty, GateMode.Gate)
    val Select   : ModeButtonLayer = ModeButtonLayer("SELECT", j.select, modeBindings = Seq.empty, GateMode.Gate)
  }

  lazy val trackBank = ext.trackBank
  trackBank.followCursorTrack(ext.cursorTrack)

  lazy val superBank: TrackBank = ext.host.createMainTrackBank(256, 8, 256)
  superBank.itemCount().markInterested()
  superBank.scrollPosition().markInterested()

  lazy val selectedClipTrack: CursorTrack = ext.host.createCursorTrack("clip track", "clip track", 0, 256, false)

  (0 until 256).foreach { i =>
    val t = superBank.getItemAt(i)
    t.clipLauncherSlotBank().addIsSelectedObserver((idx, selected) =>
      if (selected)
        selectedClipTrack.selectChannel(t))
  }

  ext.preferences.smartTracker.markInterested()
  implicit val tracker: TrackTracker = {
    if (ext.preferences.smartTracker.get())
      new SmartTracker(superBank)
    else
      new DumbTracker(superBank)
  }

  lazy val sceneBank  : SceneBank   = trackBank.sceneBank()
  lazy val masterTrack: MasterTrack = ext.host.createMasterTrack(8)

  sceneBank.canScrollForwards.markInterested()
  sceneBank.canScrollBackwards.markInterested()
  sceneBank.itemCount().markInterested()

  trackBank.cursorIndex().markInterested()

  ext.docPrefs.hideDisabled.addValueObserver { v =>
    val skip = (v != ShowHide.Show)
    trackBank.setSkipDisabledItems(skip)
    superBank.setSkipDisabledItems(skip)
  }

  {
    // meters
    masterTrack.addVuMeterObserver(128, 0, true, j.levelMeter.uL)
    masterTrack.addVuMeterObserver(128, 1, true, j.levelMeter.uR)
  }

  val stripGroup = Exclusive(levelCycle, auxLayer, controlLayer)

  // Final assembly of all mode layers
  val top       = Coexist(SimpleModeLayer("-^-", modeBindings = Vector.empty))
  val bottom    = SimpleModeLayer("_|_", modeBindings = Vector.empty)
  val unmanaged = SimpleModeLayer("_x_", modeBindings = Vector.empty)

  val graph = new ModeDGraph(
    init = Vector(levelCycle, sceneLayer, clipMatrix),
    dpad -> top,
    play -> top,
    position -> Coexist(tempoLayer),
    sceneLayer -> top,
    bottom -> Coexist(globalQuant, shiftTransport, shiftMatrix, shiftPages),
    bottom -> Exclusive(GlobalMode.Clear, GlobalMode.Duplicate, GlobalMode.Select),
    trackGroup -> Exclusive(solo, mute),
    bottom -> Coexist(clipMatrix, pageMatrix, stepSequencer),
    bottom -> stripGroup,
    bottom -> Coexist(auxGate, deviceSelector, macroLayer),
    trackGroup -> Exclusive(EIGHT.map(trackGate): _*),
    masterButton -> top,
    bottom -> Coexist(unmanaged),
  )
  /*
  - some modes are mutually exclusive (activating one will deactivate others)
  - some modes will activate others when activated (e.g. level button -> level sliders | pan sliders)
  - some modes are "main" and others "temporary" (for restoration purposes)

  can mode's activate() return an activatable?
   */

  // val newGraph = ModeCommander(
  //   clipMatrix,
  //   sceneLayer,
  //   levelCycle,
  // )
}
