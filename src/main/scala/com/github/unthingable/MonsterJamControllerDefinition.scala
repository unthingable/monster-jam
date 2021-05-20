package com.github.unthingable

import java.util.UUID
import com.bitwig.extension.api.PlatformType
import com.bitwig.extension.controller.AutoDetectionMidiPortNamesList
import com.bitwig.extension.controller.ControllerExtensionDefinition
import com.bitwig.extension.controller.api.ControllerHost


object MonsterJamExtensionDefinition {
  private val DRIVER_ID = UUID.fromString("b4b8b16c-5855-4943-a8c6-45cbdaf9aee1")
}

class MonsterJamExtensionDefinition() extends ControllerExtensionDefinition {
  override def getName = "MonsterJam"

  override def getAuthor = "unthingable"

  override def getVersion = "0.1"

  override def getId: UUID = MonsterJamExtensionDefinition.DRIVER_ID

  override def getHardwareVendor = "Native Instruments"

  override def getHardwareModel = "Maschine JAM"

  override def getRequiredAPIVersion = 13

  override def getNumMidiInPorts = 1

  override def getNumMidiOutPorts = 1

  override def listAutoDetectionMidiPortNames(list: AutoDetectionMidiPortNamesList, platformType: PlatformType): Unit =
    (1 to 4).foreach { n =>
      list.add(Array[String](s"Maschine Jam - $n"), Array[String](s"Maschine Jam - $n"))
      list.add(Array[String](s"Maschine Jam - $n Input"), Array[String](s"Maschine Jam - $n Output"))
    }

  override def createInstance(host: ControllerHost) = new MonsterJamExtension(this, host)
}
