package com.github.unthingable

import com.bitwig.extension.controller.ControllerExtension
import com.bitwig.extension.controller.api._
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.XmlMap
import com.github.unthingable.jam.surface.XmlMap.loadMap

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
  xmlMap: XmlMap
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
}

class MonsterJamExtension(val definition: MonsterJamExtensionDefinition, val host: ControllerHost) extends ControllerExtension(definition, host) {

  var ext: MonsterJamExt = null

  Util.println = host.println

  override def init(): Unit = {
    val host = getHost
    ext = MonsterJamExt(
      host,
      host.getMidiInPort(0),
      host.getMidiOutPort(0),
      host.createHardwareSurface,
      host.createCursorTrack(1, 0),
      host.createMainTrackBank(8, 8, 8),
      host.createTransport(),
      host.getDocumentState,
      host.createApplication(),
      loadMap(host)
    )

    new Jam()(ext)

    host.showPopupNotification("MonsterJam Initialized")
  }

  override def exit(): Unit = { // TODO: Perform any cleanup once the driver exits
    // For now just show a popup notification for verification that it is no longer running.
    getHost.showPopupNotification("MonsterJam Exited")
  }

  override def flush(): Unit = {
    if (ext.hw != null) ext.hw.updateHardware()
  }
}
