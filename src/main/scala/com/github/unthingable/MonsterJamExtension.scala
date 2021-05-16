package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{Application, ControllerHost, CursorTrack, DocumentState, HardwareActionBindable, HardwareSurface, MidiIn, MidiOut, SettableColorValue, TrackBank, Transport}
import com.bitwig.extension.controller.ControllerExtension
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.XmlMap
import com.github.unthingable.jam.surface.XmlMap.loadMap

import java.util.function.Supplier

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
  def binding(f: () => Unit, id: String): HardwareActionBindable = host.createAction(() => f(), () => id)
}

class MonsterJamExtension(val definition: MonsterJamExtensionDefinition, val host: ControllerHost) extends ControllerExtension(definition, host) {

  var ext: MonsterJamExt = null

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
      loadMap()
    )

    new Jam()(ext)
    //ext.midiIn.setSysexCallback { s: String => host.println(s) }

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
