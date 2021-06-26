package com.github.unthingable.bitwig

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{Track, TrackBank}
import com.github.unthingable.{MonsterJamExt, Util}

import java.nio.ByteBuffer
import scala.collection.mutable

trait TrackTracker {
  def positionForId(id: Int): Int

  def trackId(track: Track): Option[Int]

  def addRescanCallback(f: () => Unit): Unit

}
// assign ephemeral ids for up to 255 tracks using steganography on colors

/**
 * TrackTracker attempts to
 * @param bank
 */
class SmartTracker(bank: TrackBank)(implicit val ext: MonsterJamExt) extends TrackTracker {
  import TrackTracker._

  var sequence: Int = 0

  //val trackIds = Array.from(LazyList.iterate(0)(_+1).take(bank.getCapacityOfBank))
  //
  //val track2color = mutable.ArrayBuffer()

  val rescanCallbacks = mutable.ArrayDeque.empty[() => Unit]

  bank.itemCount().markInterested()

  val positionMap = mutable.HashMap.empty[Int, Int]
  val idPos = mutable.ArrayBuffer.fill(256)(-1)
  val posId = mutable.ArrayBuffer.fill(256)(-1)

  (0 until bank.getCapacityOfBank).foreach(i => {
    val track = bank.getItemAt(i)
    track.exists().markInterested()
    track.trackType().markInterested()
    track.position().markInterested()
    track.color().markInterested()
    track.name().markInterested() // TODO rm
    track.color().addValueObserver((r,g,b) => trackColorChange(track))
  })

  bank.itemCount().addValueObserver(_ => rescan())

  override def trackId(track: Track): Option[Int] = {
    val sig = signature(track)
    if (isOurs(sig))
      Some(sig & 0xff)
    else
      None
  }

  protected def trackColorChange(track: Track): Unit = {
    ext.host.scheduleTask(() => // let rescan run first if it's a new track
    if (track.exists().get) {
      val pos = track.position().get()
      val id  = posId(pos)
      if (id != -1 && !isOurs(signature(track.color().get))) {
        // otherwise we're in the middle of rescanning
        Util.println(s"genuine color change to track $pos with id $id")
        //printColor(track.color().get())
        sign(track, posId(pos).toByte)
      }
    }, 100)
  }

  protected def rescan(): Unit = {
    Util.println("rescanning superbank")
    idPos.mapInPlace(_ => -1)
    posId.mapInPlace(_ => -1)
    val newTracks = mutable.ArrayDeque.empty[Track]

    // pass 1: rescan known tracks
    (0 until bank.itemCount().get())
      .map(pos => (pos, bank.getItemAt(pos)))
      .filter(_._2.exists().get())
      .foreach { case (pos, track) =>
        val sig = signature(track)
        if (isOurs(sig)) {
          val id = sig & 0xff
          //val pos = track.position().get()
          if (idPos(id) != -1) // collision
            newTracks.addOne(track)
          else {
            Util.println(s"track ${track.name().get()} with $id -> $pos")
            idPos.update(id, pos)
            posId.update(pos, id)
          }
        } else {
          newTracks.addOne(track)
        }
      }

    // pass 2: index new tracks
    newTracks.zip(idPos.zipWithIndex.filter(_._1 == -1)).foreach { case (track, (_, id)) =>
      val pos = track.position().get()
      sign(track, id.toByte)
      idPos.update(id, pos)
      posId.update(pos, id)
    }

    while(rescanCallbacks.nonEmpty) {
      rescanCallbacks.removeHead()()
    }
  }

  override def positionForId(id: Signature): Signature = idPos(id)

  override def addRescanCallback(f: () => Unit): Unit = {
    rescanCallbacks.addOne(f)

    // in case rescan didn't fire
    ext.host.scheduleTask(() => rescanCallbacks.removeAll(_ == f), 500)
  }
}

class DumbTracker(bank: TrackBank) extends TrackTracker {
  (0 until bank.getCapacityOfBank).foreach(bank.getItemAt(_).position().markInterested())

  override def positionForId(id: Int): Int = id

  override def trackId(track: Track): Option[Int] = Some(track.position().get())

  override def addRescanCallback(f: () => Unit): Unit = ()
}

object TrackTracker {
  type Signature = Int

  val mask = 0x3f7fff00

  def signature(track: Track): Signature = {
    val c = track.color().get()
    signature(c)
  }

  def signature(c: Color): Signature = {
    asInt(asByte(c.getAlpha.toFloat):_*)
  }

  def isOurs(sig: Signature): Boolean = sig >> 8 == mask >> 8

  def sign(track: Track, id: Byte): Unit = {
    val c = track.color().get()
    val alpha: Array[Byte] = asByte(mask).updated(3, id)
    val newColor = Color.fromRGBA(c.getRed, c.getGreen, c.getBlue, asFloat(alpha:_*))
    //printColor(newColor)
    track.color().set(newColor)
  }

  def asInt(byte: Byte*): Int = ByteBuffer.wrap(byte.toArray).getInt
  def asInt(float: Float): Int = asInt(asByte(float):_*)
  def asFloat(byte: Byte*): Float = ByteBuffer.wrap(byte.toArray).getFloat
  def asFloat(int: Int): Float = asFloat(asByte(int):_*)
  def asDouble(byte: Byte*): Double = ByteBuffer.wrap(byte.toArray).getDouble
  def asDouble(int: Int): Double = asDouble(asByte(int):_*)
  def asByte(v: Int): Array[Byte] = ByteBuffer.allocate(4).putInt(v).array()
  def asByte(v: Float): Array[Byte] = ByteBuffer.allocate(4).putFloat(v).array()
  def asByte(v: Double): Array[Byte] = ByteBuffer.allocate(8).putDouble(v).array()

  //def printColor(c: Color): Unit = {
  //  Util.println((c.getRed, c.getGreen, c.getBlue, c.getAlpha).toString())
  //  Vector(c.getRed, c.getGreen, c.getBlue, c.getAlpha).foreach { v =>
  //    val arr = ByteBuffer.allocate(4).putFloat(v.toFloat).array()
  //    Util.println(arr.toSeq.map(_ & 0xff).map(s => f"$s%02x").mkString(" "))
  //  }
  //}
}
