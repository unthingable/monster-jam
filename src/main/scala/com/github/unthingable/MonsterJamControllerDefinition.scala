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

  override def getAuthor = "minortom"

  override def getVersion = "1.0"

  override def getId: UUID = MonsterJamExtensionDefinition.DRIVER_ID

  override def getHardwareVendor = "Native Instruments"

  override def getHardwareModel = "Maschine JAM"

  override def getRequiredAPIVersion = 13

  override def getNumMidiInPorts = 1

  override def getNumMidiOutPorts = 1

  override def listAutoDetectionMidiPortNames(list: AutoDetectionMidiPortNamesList, platformType: PlatformType): Unit = {
    list.add(Array[String]("Maschine Jam - 1"), Array[String]("Maschine Jam - 1"))
    list.add(Array[String]("Maschine Jam - 2"), Array[String]("Maschine Jam - 2"))
    list.add(Array[String]("Maschine Jam - 3"), Array[String]("Maschine Jam - 3"))
    list.add(Array[String]("Maschine Jam - 4"), Array[String]("Maschine Jam - 4"))
    list.add(Array[String]("Maschine Jam - 1 Input"), Array[String]("Maschine Jam - 1 Output"))
    list.add(Array[String]("Maschine Jam - 2 Input"), Array[String]("Maschine Jam - 2 Output"))
    list.add(Array[String]("Maschine Jam - 3 Input"), Array[String]("Maschine Jam - 3 Output"))
    list.add(Array[String]("Maschine Jam - 4 Input"), Array[String]("Maschine Jam - 4 Output"))
  }

  override def createInstance(host: ControllerHost) = new MonsterJamExtension(this, host)
}
