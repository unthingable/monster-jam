package com.github.unthingable

import com.bitwig.extension.controller.ControllerExtension
import com.bitwig.extension.controller.api.*
import com.github.unthingable.JamSettings.EnumSetting
import com.github.unthingable.framework.EventBus
import com.github.unthingable.framework.binding.Binder
import com.github.unthingable.framework.binding.Event
import com.github.unthingable.jam.Jam
import com.github.unthingable.util.MirroringMidiOut
import com.github.unthingable.jam.surface.XmlMap
import com.github.unthingable.jam.surface.XmlMap.loadMap

import java.io.IOException

case class MonsterPref(
  shiftRow: SettableBooleanValue,
  stepNoteInterlace: SettableBooleanValue,
  altNoteRow: SettableBooleanValue,
  stepFollow: SettableBooleanValue,
  stepNotePreview: SettableBooleanValue,
  stepKeepEnd: SettableBooleanValue,
  shiftGroup: SettableBooleanValue,
  shiftDpad: EnumSetting[JamSettings.DpadScroll],
  limitLevel: EnumSetting[JamSettings.LimitLevels],
  shiftPlay: EnumSetting[JamSettings.ShiftPlay],
  launchTolerance: SettableRangedValue,
  launchLookahead: SettableRangedValue,
  debugOutput: SettableBooleanValue,
  oscPort: SettableRangedValue,
)

case class MonsterDocPrefs(
  hideDisabled: EnumSetting[JamSettings.ShowHide]
)

case class MonsterJamExt(
  host: ControllerHost,
  midiIn: MidiIn,
  midiOut: MidiOut,
  debugMidiIn: Option[MidiIn],
  hw: HardwareSurface,
  cursorTrack: CursorTrack,
  trackBank: TrackBank,
  transport: Transport,
  document: DocumentState,
  application: Application,
  preferences: MonsterPref,
  docPrefs: MonsterDocPrefs,
  binder: Binder = new Binder(),
  events: EventBus[Event] = new EventBus(),
):
  lazy val xmlMap = loadMap(host)

  type Schedulable = (Int, () => Boolean, () => Unit) | (Int, () => Unit)

  final def sequence(tasks: List[Schedulable]): Unit =
    tasks match
      case Nil => ()
      case (wait, action) :: tt =>
        host.scheduleTask(
          () =>
            action()
            sequence(tt*)
          ,
          wait
        )
      case (wait, condition, action) :: tt =>
        host.scheduleTask(
          () =>
            if condition() then
              action()
              sequence(tt*),
          wait
        )

  final inline def sequence(tasks: Schedulable*): Unit = sequence(tasks.toList)

  // for when you need a quick action
  def a(f: => Unit): HardwareActionBindable = host.createAction(() => f, () => "")
end MonsterJamExt

class MonsterJamExtension(val definition: MonsterJamExtensionDefinition, val host: ControllerHost)
    extends ControllerExtension(definition, host):

  var ext: MonsterJamExt            = null
  private var jam: Jam              = null
  private var printer: util.Printer = null

  val preferences: Preferences = host.getPreferences

  override def init(): Unit =
    val host = getHost

    val dualPorts = MonsterJamExtensionDefinition.dualPorts

    val mirroringOut = new MirroringMidiOut(host.getMidiOutPort(0))
    if dualPorts then mirroringOut.setMirror(host.getMidiOutPort(1))

    ext = MonsterJamExt(
      host,
      host.getMidiInPort(0),
      mirroringOut,
      if dualPorts then Some(host.getMidiInPort(1)) else None,
      host.createHardwareSurface,
      host.createCursorTrack(1, 0),
      host.createMainTrackBank(8, 8, 8),
      // host.createTrackBank(8, 8, 8, false),
      host.createTransport(),
      host.getDocumentState,
      host.createApplication(),
      MonsterPref(
        preferences.getBooleanSetting("Show pretty shift commands in matrix", "Display", true),
        preferences.getBooleanSetting("Note interlace", "Step Sequencer", true),
        preferences.getBooleanSetting("Alternating note row colors", "Step Sequencer", true),
        preferences.getBooleanSetting("Step sequencer pattern follow", "Step Sequencer", false),
        preferences.getBooleanSetting("Step sequencer note preview", "Step Sequencer", false),
        preferences.getBooleanSetting("Keep note end", "Step Sequencer", true),
        preferences.getBooleanSetting("SHIFT-TRACK selects track page", "Behavior", true),
        EnumSetting(preferences, "DPAD scroll (regular/SHIFT)", "Behavior", JamSettings.DpadScroll.`page/single`),
        EnumSetting(preferences, "Limit level sliders", "Behavior", JamSettings.LimitLevels.None),
        EnumSetting(preferences, "SHIFT+PLAY", "Behavior", JamSettings.ShiftPlay.`Pause/Resume`),
        preferences.getNumberSetting("Launch Q forgiveness", "Launcher", 0, 1, 0.1, "beats", 0.5),
        preferences.getNumberSetting("Launch Q lookahead", "Launcher", 0, .5, 0.02, "beats", 0.1),
        preferences.getBooleanSetting("Verbose console output", "Debug", false),
        preferences.getNumberSetting("OSC port", "Debug", 9000, 9999, 1, "", 9200),
      ),
      MonsterDocPrefs(
        EnumSetting(host.getDocumentState, "Tracks", "Hide disabled", JamSettings.ShowHide.Show),
      ),
    )

    if ext.preferences.debugOutput.get() then
      import java.time.{Instant, ZoneId}
      import java.time.format.DateTimeFormatter
      val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS").withZone(ZoneId.systemDefault())
      printer = util.Printer(s =>
        host.println(s)
        java.lang.System.out.println(if s.nonEmpty then s"MJ ${dtf.format(Instant.now())} " + s else "")
      )
      Util.println = printer.println
    else Util.println = _ => ()

    jam = new Jam()(ext)

    if dualPorts || ext.preferences.debugOutput.get() then
      initOsc(host)

    val versionStr = s"MonsterJam ${MonsterJamExtensionDefinition.version}"
    Util.println(s"$versionStr Initialized")
    host.showPopupNotification(s"$versionStr Initialized")
  end init

  private def initOsc(host: ControllerHost): Unit =
    val oscPort      = ext.preferences.oscPort.getRaw.toInt
    val oscReplyPort = oscPort + 1
    val oscModule       = host.getOscModule
    val addressSpace    = oscModule.createAddressSpace()
    val clientAddrSpace = oscModule.createAddressSpace()
    val replyConn       = oscModule.connectToUdpServer("::ffff:127.0.0.1", oscReplyPort, clientAddrSpace)

    addressSpace.registerMethod("/mj/connect", ",i", "Client connect", (_, message) =>
      val port = message.getInt(0)
      Util.println(s"OSC client connected on port $port")
    )

    addressSpace.registerMethod("/mj/disconnect", ",i", "Client disconnect", (_, message) =>
      val port = message.getInt(0)
      Util.println(s"OSC client disconnected from port $port")
    )

    addressSpace.registerMethod("/mj/mode/state", ",", "Query mode state", (_, _) =>
      val active   = jam.graph.activeModes.mkString(",")
      val occulted = jam.graph.occultedModes.mkString(",")
      try replyConn.sendMessage("/mj/mode/state", active, occulted)
      catch case _: IOException => ()
    )

    addressSpace.registerMethod("/mj/control/submode", ",", "Query CONTROL submode", (_, _) =>
      val idx = jam.controlLayer.selected.getOrElse(-1)
      val name = jam.controlLayer.selected.map(jam.controlLayer.subModes(_).id).getOrElse("")
      try replyConn.sendMessage("/mj/control/submode", idx.asInstanceOf[Integer], name)
      catch case _: IOException => ()
    )

    oscModule.createUdpServer(oscPort, addressSpace)
    Util.println(s"OSC server listening on port $oscPort (reply: $oscReplyPort)")
  end initOsc

  override def exit(): Unit =
    printer match
      case p: util.Printer => p.timer.stop()

    getHost.showPopupNotification("MonsterJam Exited")

  override def flush(): Unit =
    if ext.hw != null then ext.hw.updateHardware()
end MonsterJamExtension
