package com.github.unthingable

import com.bitwig.extension.controller.ControllerExtension
import com.bitwig.extension.controller.api._
import com.github.unthingable.JamSettings.{EnumSetting, enumSetting}
import com.github.unthingable.JamSettings
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.binding.Binder
import com.github.unthingable.jam.surface.XmlMap
import com.github.unthingable.jam.surface.XmlMap.loadMap

case class MonsterPref(
  shiftRow: SettableBooleanValue,
  shiftGroup: SettableBooleanValue,
  shiftDpad: EnumSetting[JamSettings.DpadScroll.type],
  limitLevel: EnumSetting[JamSettings.LimitLevels.type],
  smartTracker: SettableBooleanValue,
)

case class MonsterDocPrefs(
  hideDisabled: EnumSetting[JamSettings.ShowHide.type]
)

case class MonsterJamExt(
  host: ControllerHost,
  midiIn: MidiIn,
  midiOut: MidiOut,
  hw: HardwareSurface,
  cursorTrack: CursorTrack,
  trackBank: TrackBank,
  transport: Transport,
  document: DocumentState,
  application: Application,
  preferences: MonsterPref,
  docPrefs: MonsterDocPrefs,
  xmlMap: XmlMap,
  binder: Binder = new Binder(),
) {
  type Schedulable = (Int, () => Boolean, () => Unit)
  final def run(tasks: Schedulable*): Unit = {
    tasks match {
      case Nil => ()
      case (wait, condition, action) :: tt =>
        host.scheduleTask(() => if (condition()) {
          action()
          run(tt: _*)
        }, wait)
    }
  }

  // for when you need a quick action
  def a(f: => Unit): HardwareActionBindable = host.createAction(() => f, () => "")
}

class MonsterJamExtension(val definition: MonsterJamExtensionDefinition, val host: ControllerHost) extends ControllerExtension(definition, host) {

  var ext: MonsterJamExt = null
  private var jam: Jam = null

  val printer = new Printer(host.println)
  Util.println = printer.println

  val preferences: Preferences = host.getPreferences

  override def init(): Unit = {
    val host = getHost
    ext = MonsterJamExt(
      host,
      host.getMidiInPort(0),
      host.getMidiOutPort(0),
      host.createHardwareSurface,
      host.createCursorTrack(1, 0),
      host.createMainTrackBank(8, 8, 8),
      //host.createTrackBank(8, 8, 8, false),
      host.createTransport(),
      host.getDocumentState,
      host.createApplication(),
      MonsterPref(
        preferences.getBooleanSetting("Show pretty shift commands in matrix", "Options", true),
        preferences.getBooleanSetting("SHIFT-TRACK selects track page", "Options", true),
        enumSetting(preferences, "DPAD scroll (regular/SHIFT)", "Options", JamSettings.DpadScroll.RegularPage),
        enumSetting(preferences, "Limit level sliders", "Options", JamSettings.LimitLevels.None),
        preferences.getBooleanSetting("Enable track tracker", "Options", true),
      ),
      MonsterDocPrefs(
        enumSetting(host.getDocumentState, "Tracks", "Hide disabled", JamSettings.ShowHide.Show),
      ),
      loadMap(host)
    )

    jam = new Jam()(ext)

    host.showPopupNotification("MonsterJam Initialized")
  }

  override def exit(): Unit = {
    // For now just show a popup notification for verification that it is no longer running.
    getHost.showPopupNotification("MonsterJam Exited")
  }

  override def flush(): Unit = {
    if (ext.hw != null) ext.hw.updateHardware()
  }
}
