package com.github.unthingable.jam

import com.bitwig.extension.controller.api.Track
import com.bitwig.extension.controller.api.TrackBank
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.Ref

import com.bitwig.extension.controller.api.SettableStringValue
import com.github.unthingable.jam.stepSequencer.state.SeqState

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import java.io.ObjectStreamClass
import java.lang.reflect.Method
import java.util.UUID
import scala.collection.mutable
import scala.util.Try

@SerialVersionUID(1L)
case class TrackId(value: String) extends AnyVal derives CanEqual

/** Old TrackId format for deserializing legacy data */
@SerialVersionUID(-2378758515367024156L)
case class LegacyTrackId(value: Int) extends AnyVal derives CanEqual

trait TrackTracker:
  protected def bank: TrackBank

  def idForPosition(pos: Int): Option[TrackId]

  def trackId(track: Track): Option[TrackId]

  def getItemAt(id: TrackId): Option[Track]

  def idList: Seq[Option[TrackId]]

  def idMap: Seq[(TrackId, Int)]

/** Primary tracker using the official Channel.channelId() API (v20+). */
class ChannelIdTracker(val bank: TrackBank)(using ext: MonsterJamExt) extends TrackTracker, Util:
  bank.fullView.foreach(_.channelId().markInterested())

  override def trackId(track: Track): Option[TrackId] =
    val id = track.channelId().get()
    if id != null && id.nonEmpty then Some(TrackId(id)) else None

  override def idForPosition(pos: Int): Option[TrackId] =
    if pos >= 0 && pos < bank.getCapacityOfBank then trackId(bank.getItemAt(pos))
    else None

  override def getItemAt(id: TrackId): Option[Track] =
    bank.itemView.find(t => trackId(t).contains(id))

  override def idList: Seq[Option[TrackId]] =
    bank.itemView.map(trackId).toVector

  override def idMap: Seq[(TrackId, Int)] =
    idList.zipWithIndex.flatMap((opt, idx) => opt.map((_, idx)))
end ChannelIdTracker

/** Track ephemeral IDs as hashcodes of Track objects under proxies. Unlike SmartTracker which was stable, UnsafeTracker
  * does not preserve IDs between host restarts.
  *
  * LEGACY: Kept for migration support. Not instantiated at runtime — ChannelIdTracker is used instead.
  */
class UnsafeTracker(val bank: TrackBank)(using ext: MonsterJamExt) extends TrackTracker, Util:

  given Util.SelfEqual[Option[Method]] = CanEqual.derived

  private type MRef = Ref[Option[Method]]
  private var idM: MRef     = Ref(None)
  private var targetM: MRef = Ref(None)

  bank.fullView.zipWithIndex
    .foreach {
      case (t, idx) =>
        t.position().markInterested()
        t.name().markInterested()
    }

  bank
    .itemCount()
    .addValueObserver(_ =>
      // check for collisions
      val hashes = bank.itemView.flatMap(idForBankTrack).toVector
      if hashes.distinct.size != hashes.size then // && ext.preferences.smartTracker.get())
        ext.host.showPopupNotification("Track ID hash collision detected, superscenes might not work")
        val dups = hashes.zipWithIndex
          .map((id, idx) =>
            val track = bank.getItemAt(idx)
            Util.println(s"$idx $id ${track.name().get}")
            (id, track)
          )
          .groupBy(_._1)
          .values
          .filter(_.length > 1)
        // .tapEach(v => Util.println(v.toString()))
        val names = dups.flatten.map(_._2.name().get()).toVector.distinct.mkString(",")
        ext.host.showPopupNotification(s"Track ID hash collision: $names")
    )

  val ids = mutable.ArraySeq.from(0 until bank.getCapacityOfBank())

  override inline def trackId(track: Track): Option[TrackId] =
    val pos = track.position().get
    idForPosition(pos)

  override inline def idForPosition(pos: Int): Option[TrackId] =
    if pos != -1 then
      val st: Track = bank.getItemAt(pos)
      idForBankTrack(st)
    else None

  override inline def getItemAt(id: TrackId): Option[Track] =
    bank.itemView.find(t => trackId(t).contains(id))

  // cache method references
  private def getOr(mref: MRef, method: => Option[Method]): Option[Method] =
    val value = mref.get
    if value.isDefined then value
    else if method.isDefined then
      mref.set(method)
      method
    else None

  private def idForBankTrack(st: Track): Option[TrackId] =
    val uid = for
      tMethod: Method <- getOr(targetM, Option(st.getClass.getMethod("getTarget")))
      tObj: Object    <- Option(tMethod.invoke(st))
      idMethod: Method <- getOr(
        idM,
        tObj
          .getClass()
          .getMethods()
          .filter(_.getReturnType().equals(classOf[UUID]))
          .lastOption
      ) // we can only hope it's the right one, no way to know for sure
    yield idMethod.invoke(tObj).asInstanceOf[UUID]

    // UnsafeTracker now returns channelId-compatible String UUIDs
    uid.map(u => TrackId(u.toString))
  end idForBankTrack

  inline def idList: Seq[Option[TrackId]] =
    bank.itemView.map(idForBankTrack).toVector

  inline def idMap: Seq[(TrackId, Int)] =
    idList.zipWithIndex.map((a, b) => a.map((_, b))).flatten
end UnsafeTracker

/** Migration utilities for converting old Int-based TrackIds to new String-based ones. */
object TrackIdMigration:
  /** Compute legacy Int hash for a track via reflection (one-shot, no subscriptions needed). */
  private def legacyHashForTrack(track: Track): Option[Int] =
    val uid = for
      tMethod  <- Option(track.getClass.getMethod("getTarget"))
      tObj     <- Option(tMethod.invoke(track))
      idMethod <- tObj.getClass.getMethods.filter(_.getReturnType.equals(classOf[UUID])).lastOption
    yield idMethod.invoke(tObj).asInstanceOf[UUID]
    uid.map(_.hashCode())

  /** Build mapping: old Int hash -> new String UUID */
  def buildMapping(bank: TrackBank, tracker: ChannelIdTracker): Map[Int, String] =
    (0 until bank.getCapacityOfBank).flatMap { pos =>
      for
        oldHash <- legacyHashForTrack(bank.getItemAt(pos))
        newId   <- tracker.idForPosition(pos)
      yield oldHash -> newId.value
    }.toMap

  /** Deserialize old format, remapping class descriptor TrackId -> LegacyTrackId.
    * Uses readClassDescriptor (not resolveClass) to fully replace the stream descriptor,
    * avoiding "local class name incompatible with stream class name" errors.
    */
  def deserializeLegacy[A](s: String): Either[Throwable, A] =
    Try {
      val bis = new ByteArrayInputStream(java.util.Base64.getDecoder.decode(s))
      val ois = new ObjectInputStream(bis):
        override def readClassDescriptor(): ObjectStreamClass =
          val desc = super.readClassDescriptor()
          if desc.getName == "com.github.unthingable.jam.TrackId" then
            ObjectStreamClass.lookup(classOf[LegacyTrackId])
          else desc
      val obj = ois.readObject()
      ois.close()
      obj.asInstanceOf[A]
    }.toEither

  /** Attempt to migrate a serialized string. Returns Some(newData) if migration happened, None if not needed. */
  def migrateString[A](s: String, remap: (A, Map[Int, String]) => Any, mapping: => Map[Int, String]): Option[String] =
    if s == null || s.isEmpty then None
    else Util.deserialize[A](s) match
      case Right(_) => None  // already new format
      case Left(_) =>
        deserializeLegacy[A](s).toOption.map(legacy => Util.serialize(remap(legacy, mapping)))
end TrackIdMigration

/** Rewrites old Int-based TrackId data in document settings to new String-based format.
  * Instantiated in Jam, called on init and project switch before components read the settings.
  */
class TrackIdMigrator(ct: ChannelIdTracker, sceneStore: SettableStringValue, stepStore: SettableStringValue):
  def migrate(): Unit =
    lazy val mapping = TrackIdMigration.buildMapping(ct.bank, ct)
    // Abort if no track IDs are available yet (e.g. during init before first update cycle)
    if mapping.isEmpty then return

    type LegacySceneData = mutable.ArraySeq[Map[LegacyTrackId, Int]]
    TrackIdMigration
      .migrateString[LegacySceneData](
        sceneStore.get(),
        (legacy, m) =>
          mutable.ArraySeq.from(legacy.map(_.flatMap { case (LegacyTrackId(hash), clip) =>
            m.get(hash).map(TrackId(_) -> clip)
          })),
        mapping
      )
      .foreach { data =>
        sceneStore.set(data)
        Util.println("Migrated superscene track IDs")
      }

    type LegacyStepData = Seq[(LegacyTrackId, SeqState)]
    TrackIdMigration
      .migrateString[LegacyStepData](
        stepStore.get(),
        (legacy, m) =>
          legacy.flatMap { case (LegacyTrackId(hash), st) =>
            m.get(hash).map { uuid =>
              val newId = TrackId(uuid)
              (newId, st.copy(tid = Some(newId)))
            }
          },
        mapping
      )
      .foreach { data =>
        stepStore.set(data)
        Util.println("Migrated step sequencer track IDs")
      }
end TrackIdMigrator
