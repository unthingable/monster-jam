package com.github.unthingable.jam

import com.bitwig.extension.controller.api._
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.JamSettings.ShowHide
import com.github.unthingable.jam.Graph.{Coexist, Exclusive, ModeDGraph}
import com.github.unthingable.jam.surface._
import com.github.unthingable.jam.layer._


/*
Behavior definition for surface controls
 */


class Jam(implicit val ext: MonsterJamExt)
  extends BindingDSL
  with Aux with TransportL with Level with Dpad with TrackL
  with ClipMatrix with Shift with Control with MacroL with SceneL {

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

  val superBank: TrackBank = ext.host.createMainTrackBank(256, 8, 256)
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

  ext.docPrefs.hideDisabled.addValueObserver { v =>
    val skip = (v != ShowHide.Show)
    trackBank.setSkipDisabledItems(skip)
    superBank.setSkipDisabledItems(skip)
  }
  //sceneBank.setIndication(true)

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
    masterButton -> top,
    bottom -> Coexist(unmanaged),
  )
}
