package com.github.unthingable

import com.bitwig.extension.controller.ControllerExtension
import com.bitwig.extension.controller.api.*
import com.github.unthingable.JamSettings.EnumSetting
import com.github.unthingable.JamSettings
import com.github.unthingable.framework.EventBus
import com.github.unthingable.jam.Jam
import com.github.unthingable.framework.binding.{Binder, Event}
import com.github.unthingable.framework.mode.ModeCommander
import com.github.unthingable.jam.surface.XmlMap
import com.github.unthingable.jam.surface.XmlMap.loadMap

case class MonsterPref(
  shiftRow: SettableBooleanValue,
  shiftGroup: SettableBooleanValue,
  shiftDpad: EnumSetting[JamSettings.DpadScroll],
  limitLevel: EnumSetting[JamSettings.LimitLevels],
  smartTracker: SettableBooleanValue,
  debugOutput: SettableBooleanValue,
)

case class MonsterDocPrefs(
  hideDisabled: EnumSetting[JamSettings.ShowHide]
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
  events: EventBus[Event] = new EventBus(),
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
        EnumSetting(preferences, "DPAD scroll (regular/SHIFT)", "Options", JamSettings.DpadScroll.`page/single`),
        EnumSetting(preferences, "Limit level sliders", "Options", JamSettings.LimitLevels.None),
        preferences.getBooleanSetting("Enable track tracker", "Options", true),
        preferences.getBooleanSetting("Verbose console output", "Debug", false),
      ),
      MonsterDocPrefs(
        EnumSetting(host.getDocumentState, "Tracks", "Hide disabled", JamSettings.ShowHide.Show),
      ),
      loadMap(host)
    )

    if (ext.preferences.debugOutput.get())
      val printer = util.Printer(host.println)
      Util.println = printer.println
    else
      Util.println = _ => ()

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
