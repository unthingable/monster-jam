package com.github.unthingable.jam

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{Track, TrackBank}
import com.github.unthingable.{MonsterJamExt, Util}

import java.nio.ByteBuffer
import scala.collection.mutable
import java.lang.reflect.Method
import com.github.unthingable.framework.Ref
import java.util.UUID

case class TrackId(value: Int) extends AnyVal

trait TrackTracker {
  protected def bank: TrackBank

  // def positionForId(id: TrackId): Option[Int]

  def idForPosition(pos: Int): Option[TrackId]

  def trackId(track: Track): Option[TrackId]

  // def addRescanCallback(f: () => Unit): Unit

  def getItemAt(id: TrackId): Option[Track] //= positionForId(id).map(bank.getItemAt)

  def idList: Seq[Option[TrackId]]
  
  def idMap: Seq[(TrackId, Int)]
}

/** SmartTracker attempts to identify tracks by injecting identifiers into their color's Alpha
  * channel.
  *
  * @deprecated
  *   broken as of 4.3
  */
// class SmartTracker(val bank: TrackBank)(implicit val ext: MonsterJamExt) extends TrackTracker {
//   import TrackTracker._

//   var sequence: Int = 0

//   private val rescanCallbacks = mutable.ArrayDeque.empty[() => Unit]

//   bank.itemCount().markInterested()

//   private val idPos = mutable.ArrayBuffer.fill(256)(-1)
//   private val posId = mutable.ArrayBuffer.fill(256)(-1)

//   (0 until bank.getCapacityOfBank).foreach { i =>
//     val track = bank.getItemAt(i)
//     track.exists().markInterested()
//     track.trackType().markInterested()
//     track.position().markInterested()
//     track.color().markInterested()
//     track.name().markInterested() // TODO rm
//     track.color().addValueObserver((r, g, b) => trackColorChange(track))
//   }

//   bank.itemCount().addValueObserver(_ => rescan())

//   override def trackId(track: Track): Option[TrackId] = {
//     val sig = signature(track)
//     if (isOurs(sig))
//       Some(TrackId(sig & 0xff))
//     else
//       None
//   }

//   protected def trackColorChange(track: Track): Unit =
//     ext.host.scheduleTask(
//       () => // let rescan run first if it's a new track
//         if (track.exists().get) {
//           val pos = track.position().get()
//           val id  = posId(pos)
//           if (id != -1 && !isOurs(signature(track.color().get))) {
//             // otherwise we're in the middle of rescanning
//             Util.println(s"genuine color change to track $pos with id $id")
//             // printColor(track.color().get())
//             sign(track, id.toByte)
//           }
//         },
//       100
//     )

//   protected def rescan(): Unit = {
//     Util.println("rescanning superbank")
//     idPos.mapInPlace(_ => -1)
//     posId.mapInPlace(_ => -1)
//     val newTracks = mutable.ArrayDeque.empty[Track]

//     // pass 1: rescan known tracks
//     (0 until bank.itemCount().get())
//       .map(pos => (pos, bank.getItemAt(pos)))
//       .filter(_._2.exists().get())
//       .foreach {
//         case (pos, track) =>
//           val sig = signature(track)
//           if (isOurs(sig)) {
//             val id = sig & 0xff
//             // val pos = track.position().get()
//             if (idPos(id) != -1) // collision
//               newTracks.addOne(track)
//             else {
//               Util.println(s"track ${track.name().get()} with $id -> $pos")
//               idPos.update(id, pos)
//               posId.update(pos, id)
//             }
//           } else {
//             newTracks.addOne(track)
//           }
//       }

//     // pass 2: index new tracks
//     newTracks.zip(idPos.zipWithIndex.filter(_._1 == -1)).foreach {
//       case (track, (_, id)) =>
//         val pos = track.position().get()
//         sign(track, id.toByte)
//         idPos.update(id, pos)
//         posId.update(pos, id)
//     }

//     while (rescanCallbacks.nonEmpty)
//       rescanCallbacks.removeHead()()
//   }

//   def positionForId(id: TrackId): Option[Int] = {
//     val pos = idPos(id.value)
//     if (pos == -1)
//       None
//     else
//       Some(pos)
//   }

//   def addRescanCallback(f: () => Unit): Unit = {
//     rescanCallbacks.addOne(f)

//     // in case rescan didn't fire
//     ext.host.scheduleTask(() => rescanCallbacks.removeAll(_ == f), 500)
//   }

//   override def idForPosition(pos: Signature): Option[TrackId] = {
//     val id = posId(pos)
//     if (id == -1) None else Some(TrackId(id))
//   }

//   def getItemAt(id: TrackId): Option[Track] = positionForId(id).map(bank.getItemAt)

//   def idList: Seq[Option[TrackId]] = ???
  
//   def idMap: Seq[(TrackId, Int)] = ???

// }

// positions and IDs are the same
// class DumbTracker(val bank: TrackBank) extends TrackTracker {
//   (0 until bank.getCapacityOfBank).foreach(bank.getItemAt(_).position().markInterested())

//   override def trackId(track: Track): Option[TrackId] = Some(TrackId(track.position().get()))

//   override def idForPosition(pos: Int): Option[TrackId] = Some(TrackId(pos))

//   override def getItemAt(id: TrackId): Option[Track] = Some(bank.getItemAt(id.value))

//   def idList: Seq[Option[TrackId]] = idMap.map(x => Some(x._1))
  
//   def idMap: Seq[(TrackId, Int)] = (0 until bank.getCapacityOfBank()).map(i => (TrackId(i), i))
// }

/** Track ephemeral IDs as hashcodes of Track objects under proxies.
 * Unlike SmartTracker which was stable, UnsafeTracker does not preserve IDs between host restarts.
 */
class UnsafeTracker(val bank: TrackBank)(using ext: MonsterJamExt) extends TrackTracker {

  private val bankRange = 0 until bank.getCapacityOfBank()
  private type MRef = Ref[Option[Method]]
  private var idM: MRef = Ref(None)
  private var targetM: MRef = Ref(None)

  bankRange
    .map(bank.getItemAt)
    .zipWithIndex
    .foreach { case (t, idx) =>
      t.position().markInterested()
      t.name().markInterested()
    }
  
  bank.itemCount().addValueObserver(_ =>
    // check for collisions
    val hashes = bankRange.map(bank.getItemAt).flatMap(idForBankTrack)
    if (hashes.distinct.size != hashes.size && ext.preferences.smartTracker.get())
      ext.host.showPopupNotification("Track ID hash collision detected, superscenes might not work")
  )

  val ids = mutable.ArraySeq.from(0 until bank.getCapacityOfBank())

  override inline def trackId(track: Track): Option[TrackId] =
    val pos = track.position().get
    idForPosition(pos)

  override inline def idForPosition(pos: Int): Option[TrackId] =     
    if (pos != -1)
      val st: Track = bank.getItemAt(pos)
      idForBankTrack(st)
    else None

  override inline def getItemAt(id: TrackId): Option[Track] =
    LazyList.from(bankRange).map(bank.getItemAt).find(t => trackId(t).contains(id))

  // cache method references
  private def getOr(mref: MRef, method: => Option[Method]): Option[Method] =
    val value = mref.get
    if (value.isDefined) value
    else
      if (method.isDefined)
        mref.set(method)
        method
      else
        None

  private def idForBankTrack(st: Track): Option[TrackId] =
    val uid = for {
      tMethod: Method <- getOr(targetM, Option(st.getClass.getMethod("getTarget")))
      tObj: Object <- Option(tMethod.invoke(st))
      idMethod: Method <- getOr(idM, 
        tObj.getClass().getMethods()
        .filter(_.getReturnType().equals(classOf[UUID]))
        .headOption) // let's hope there is just one
    } yield idMethod.invoke(tObj).asInstanceOf[UUID]

    val id = uid.map(_.hashCode())

    // Util.println(s"id for ${st.name().get}: $uid $id")
    id.map(TrackId.apply)

  inline def idList: Seq[Option[TrackId]] = 
    LazyList.from(bankRange).map(idx => idForBankTrack(bank.getItemAt(idx)))
  
  inline def idMap: Seq[(TrackId, Int)] = 
    idList.zipWithIndex.map{case (a, b) => a.map((_, b))}.flatten
}

object TrackTracker {
  type Signature = Int

  val mask = 0x3f7fff00

  def signature(track: Track): Signature = {
    val c = track.color().get()
    signature(c)
  }

  def signature(c: Color): Signature =
    asInt(asByte(c.getAlpha.toFloat): _*)

  def isOurs(sig: Signature): Boolean = sig >> 8 == mask >> 8

  def sign(track: Track, id: Byte): Unit = {
    val c                  = track.color().get()
    val alpha: Array[Byte] = asByte(mask).updated(3, id)
    val newColor           = Color.fromRGBA(c.getRed, c.getGreen, c.getBlue, asFloat(alpha: _*))
    // printColor(newColor)
    track.color().set(newColor)
  }

  def asInt(byte: Byte*): Int        = ByteBuffer.wrap(byte.toArray).getInt
  def asInt(float: Float): Int       = asInt(asByte(float): _*)
  def asFloat(byte: Byte*): Float    = ByteBuffer.wrap(byte.toArray).getFloat
  def asFloat(int: Int): Float       = asFloat(asByte(int): _*)
  def asDouble(byte: Byte*): Double  = ByteBuffer.wrap(byte.toArray).getDouble
  def asDouble(int: Int): Double     = asDouble(asByte(int): _*)
  def asByte(v: Int): Array[Byte]    = ByteBuffer.allocate(4).putInt(v).array()
  def asByte(v: Float): Array[Byte]  = ByteBuffer.allocate(4).putFloat(v).array()
  def asByte(v: Double): Array[Byte] = ByteBuffer.allocate(8).putDouble(v).array()

}
