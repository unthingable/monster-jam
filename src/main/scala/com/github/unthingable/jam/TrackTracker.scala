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

/** Track ephemeral IDs as hashcodes of Track objects under proxies.
 * Unlike SmartTracker which was stable, UnsafeTracker does not preserve IDs between host restarts.
 */
class UnsafeTracker(val bank: TrackBank)(using ext: MonsterJamExt) extends TrackTracker, Util {

  private type MRef = Ref[Option[Method]]
  private var idM: MRef = Ref(None)
  private var targetM: MRef = Ref(None)

  bank
    .fullView
    .zipWithIndex
    .foreach { case (t, idx) =>
      t.position().markInterested()
      t.name().markInterested()
    }
  
  bank.itemCount().addValueObserver(_ =>
    // check for collisions
    val hashes = bank.view.flatMap(idForBankTrack).toVector
    if (hashes.distinct.size != hashes.size) // && ext.preferences.smartTracker.get())
      ext.host.showPopupNotification("Track ID hash collision detected, superscenes might not work")
      val dups = hashes.zipWithIndex.map((id, idx) => 
        val track = bank.getItemAt(idx)
        Util.println(s"$idx $id ${track.name().get}")
        (id, track)
      ).groupBy(_._1)
      .values.filter(_.length > 1)
      // .tapEach(v => Util.println(v.toString()))
      val names = dups.flatten.map(_._2.name().get()).toVector.distinct.mkString(",")
      ext.host.showPopupNotification(s"Track ID hash collision: $names")      
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
    bank.view.find(t => trackId(t).contains(id))

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
        // .tapEach(m => Util.println(s"$st $m"))
        .lastOption) // we can only hope it's the right one, no way to know for sure
    } yield idMethod.invoke(tObj).asInstanceOf[UUID]

    val id = uid.map(_.hashCode())

    // Util.println(s"id for ${st.name().get}: $uid $id")
    id.map(TrackId.apply)

  inline def idList: Seq[Option[TrackId]] = 
    bank.view.map(idForBankTrack).toVector
  
  inline def idMap: Seq[(TrackId, Int)] = 
    idList.zipWithIndex.map{(a, b) => a.map((_, b))}.flatten
}

