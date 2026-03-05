package com.github.unthingable

import com.bitwig.extension.api.PlatformType
import com.bitwig.extension.controller.AutoDetectionMidiPortNamesList
import com.bitwig.extension.controller.ControllerExtensionDefinition
import com.bitwig.extension.controller.api.ControllerHost

import java.util.UUID

object MonsterJamExtensionDefinition:
  private val DRIVER_ID = UUID.fromString("b4b8b16c-5855-4943-a8c6-45cbdaf9aee1")

  private val props =
    val p = new java.util.Properties()
    val stream = getClass.getResourceAsStream("/monsterjam.properties")
    if stream != null then
      try p.load(stream)
      finally stream.close()
    p

  val version: String = props.getProperty("version", "unknown")
  val dualPorts: Boolean = props.getProperty("dualPorts", "false").toBoolean

class MonsterJamExtensionDefinition() extends ControllerExtensionDefinition:
  override def getName = "MonsterJam"

  override def getAuthor = "unthingable"

  override def getVersion = MonsterJamExtensionDefinition.version

  override def getId: UUID = MonsterJamExtensionDefinition.DRIVER_ID

  override def getHardwareVendor = "Native Instruments"

  override def getHardwareModel = "Maschine JAM"

  override def getRequiredAPIVersion = 21

  override def getNumMidiInPorts = if MonsterJamExtensionDefinition.dualPorts then 2 else 1

  override def getNumMidiOutPorts = if MonsterJamExtensionDefinition.dualPorts then 2 else 1

  override def listAutoDetectionMidiPortNames(list: AutoDetectionMidiPortNamesList, platformType: PlatformType): Unit =
    val extra = if MonsterJamExtensionDefinition.dualPorts then Array("") else Array.empty[String]
    (1 to 4).foreach { n =>
      list.add(Array(s"Maschine Jam - $n") ++ extra, Array(s"Maschine Jam - $n") ++ extra)
      list.add(Array(s"Maschine Jam - $n Input") ++ extra, Array(s"Maschine Jam - $n Output") ++ extra)
    }

  override def createInstance(host: ControllerHost) = new MonsterJamExtension(this, host)
end MonsterJamExtensionDefinition
