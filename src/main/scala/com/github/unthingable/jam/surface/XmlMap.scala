package com.github.unthingable.jam.surface

import com.bitwig.extension.api.PlatformType
import com.bitwig.extension.controller.api.ControllerHost
import com.github.unthingable.Util
import com.github.unthingable.jam.surface.XmlMap.controlInfo

import java.io.File
import scala.io.Source
import scala.util.Try
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.XML

sealed trait MidiEvent:
  val value: Int
case class CC(value: Int)   extends MidiEvent
case class Note(value: Int) extends MidiEvent
//case class Poly(value: Int) extends MidiEvent
case class MidiInfo(id: String, channel: Int, event: MidiEvent)

/*
 * Crude but effective convenience parser for NI JAM controller mapping,
 * because why define them again if we can get them from the same mapping.
 */
case class XmlMap(e: Elem):
  val sceneIndex: String = (e \\ "scenePages" \ "current_index").text
  val ioIndex: String    = (e \\ "iolevelPages" \ "current_index").text
  val touchIndex: String = (e \\ "touchstripPages" \ "current_index").text

  lazy val mainElems: NodeSeq = e \\ "controls"
  lazy val matrixElems: Node  = (e \\ "scenePages" \\ "scene")(sceneIndex.toInt)
  lazy val masterElems: Node  = (e \\ "iolevelPages" \\ "page")(ioIndex.toInt)
  lazy val touchElems: Node   = (e \\ "touchstripPages" \\ "page")(touchIndex.toInt)
  lazy val allElems: NodeSeq  = mainElems ++ matrixElems ++ masterElems ++ touchElems

  def find(id: String, etype: String, elem: NodeSeq = e): Option[Node] = (elem \\ etype).find(_ \@ "id" == id)

  def findControl(id: String, etype: String, elem: NodeSeq = e): Option[MidiInfo] =
    find(id, etype, elem).flatMap(controlInfo)

  def button(id: String, elem: NodeSeq = e): MidiInfo = findControl(id, "button", elem).get
  def led(id: String, elem: NodeSeq = e): MidiInfo    = findControl(id, "led", elem).get
  def knob(id: String, elem: NodeSeq = e): MidiInfo   = findControl(id, "knob", elem).get
  def wheel(id: String, elem: NodeSeq = e): MidiInfo  = findControl(id, "wheel", elem).get
end XmlMap

object XmlMap:
  val file = "Bitwig Studio ext.ncmj"

  given Util.SelfEqual[PlatformType] = CanEqual.derived

  def loadMap(host: ControllerHost): XmlMap =
    val extDir = host.getPlatformType match
      case PlatformType.WINDOWS =>
        val userProfile = System.getenv("USERPROFILE").replace("\\", "/")
        userProfile + "/Documents/Bitwig Studio/Extensions/"
      case PlatformType.MAC =>
        System.getProperty("user.home") + "/Documents/Bitwig Studio/Extensions/"
      case _ =>
        throw new IllegalArgumentException("Unknown/unsupported platform")

    val dir = new File(extDir)
    Util.println(s"Loading optional .ncmj extensions from: $dir")
    val foundFiles: Option[File] =
      Option(dir.listFiles((_, name) => name.endsWith(".ncmj")))
        .map(_.headOption)
        .flatten

    val source = Vector(
      // bring your own
      Try {
        val f   = foundFiles.get
        val ret = XML.loadFile(f)
        host.println(s"Loaded file ${f.getName}")
        ret
      },
      // bundled mapping file
      Try(XML.load(getClass.getClassLoader.getResourceAsStream(file))),
      // file from resources (development)
      Try(XML.load(Source.fromResource(file).reader())),
    )

    val xml = source.find(_.isSuccess).head
    XmlMap(xml.get)
  end loadMap

  // all controls are 0/127 on/off or min/max
  def controlInfo(n: Node): Option[MidiInfo] =
    val id      = n \@ "id"
    val channel = (n \ "channel").text
    val cc      = (n \ "controller").text
    val note    = (n \ "note").text
    // val poly = (n \ "polyat").text

    (id, channel, cc, note) match
      case ("", "", _, _)     => None
      case _ if cc.nonEmpty   => Some(MidiInfo(id, channel.toInt, CC(cc.toInt)))
      case _ if note.nonEmpty => Some(MidiInfo(id, channel.toInt, Note(note.toInt)))
      // case _ if poly.nonEmpty => Some(MidiInfo(id, channel.toInt, Poly(note.toInt)))
      case _ => None
end XmlMap
