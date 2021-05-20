package com.github.unthingable.jam.surface

import com.github.unthingable.jam.surface.XmlMap.controlInfo

import scala.io.Source
import scala.util.Try
import scala.xml.{Elem, Node, NodeSeq, XML}

sealed trait MidiEvent { val value: Int }
case class CC(value: Int) extends MidiEvent
case class Note(value: Int) extends MidiEvent
//case class Poly(value: Int) extends MidiEvent
case class MidiInfo(id: String, channel: Int, event: MidiEvent)

/*
 * Convenience parser for NI JAM controller mapping, because why define them again
 * if we can get them from the same mapping.
 */
case class XmlMap(e: Elem) {
  /*
    Page indexes look random in the original mapping, no idea why but it is what it is.
    This works with the bundled JAM mapping.
   */
  lazy val mainElems = e \\ "controls"
  lazy val matrixElems = (e \\ "scene").filter(_ \@ "name" == "Matrix Button Page 3")
  lazy val masterElems = (e \\ "iolevelPages" \ "page").filter(_ \@ "name" == "Master")
  lazy val touchElems = (e \\ "touchstripPages" \ "page").filter(_ \@ "name" == "Touchstrip Page H")
  lazy val allElems = mainElems ++ matrixElems ++ masterElems ++ touchElems

  def find(id: String, etype: String, elem: NodeSeq = e): Option[Node] =
    (elem \\ etype).find(_ \@ "id" == id)

  def findControl(id: String, etype: String, elem: NodeSeq = e): Option[MidiInfo] =
    find(id, etype, elem).flatMap(controlInfo)

  def button(id: String, elem: NodeSeq = e): MidiInfo = findControl(id, "button", elem).get
  def led(id: String, elem: NodeSeq = e): MidiInfo = findControl(id, "led", elem).get
  def knob(id: String, elem: NodeSeq = e): MidiInfo = findControl(id, "knob", elem).get
  def wheel(id: String, elem: NodeSeq = e): MidiInfo = findControl(id, "wheel", elem).get
}

object XmlMap {
  val file = "Bitwig Studio ext.ncmj"

  def loadMap(): XmlMap = {
    val xml = Try(
      XML.load(getClass.getClassLoader.getResourceAsStream(file)))
      .orElse(
        Try(XML.load(Source.fromResource(file).reader())))
    XmlMap(xml.get)
  }

  // all controls are 0/127 on/off or min/max
  def controlInfo(n: Node): Option[MidiInfo] = {
    val id = n \@ "id"
    val channel = (n \ "channel").text
    val cc = (n \ "controller").text
    val note = (n \ "note").text
    //val poly = (n \ "polyat").text

    (id, channel, cc, note) match {
      case ("","",_,_) => None
      case _ if cc.nonEmpty => Some(MidiInfo(id, channel.toInt, CC(cc.toInt)))
      case _ if note.nonEmpty => Some(MidiInfo(id, channel.toInt, Note(note.toInt)))
      //case _ if poly.nonEmpty => Some(MidiInfo(id, channel.toInt, Poly(note.toInt)))
      case _ => None
    }
  }
}
