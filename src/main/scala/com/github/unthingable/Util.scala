package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.Bank
import com.bitwig.extension.controller.api.ObjectProxy
import com.github.unthingable.jam.surface.JamColor.JamColorBase.CYAN
import com.github.unthingable.jam.surface.JamColor.JamColorBase.FUCHSIA
import com.github.unthingable.jam.surface.JamColor.JamColorBase.GREEN
import com.github.unthingable.jam.surface.JamColor.JamColorBase.LIME
import com.github.unthingable.jam.surface.JamColor.JamColorBase.MAGENTA
import com.github.unthingable.jam.surface.JamColor.JamColorBase.ORANGE
import com.github.unthingable.jam.surface.JamColor.JamColorBase.RED
import com.github.unthingable.jam.surface.JamColor.JamColorBase.YELLOW

import java.awt.event.ActionEvent
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.nio.ByteBuffer
import java.time.Duration
import java.time.Instant
import javax.swing.Timer
import scala.annotation.targetName
import scala.collection.IndexedSeqView
import scala.collection.mutable
import scala.deriving.Mirror
import scala.util.Try

trait Math:
  import Ordering.Implicits.*
  import Integral.Implicits.*

  /** Find next and previous multiple of step */
  trait Steppable[A]:
    def next(n: A, step: A): A
    def prev(n: A, step: A): A

  given s_int[A](using int: Integral[A]): Steppable[A] with
    inline def next(n: A, step: A) = if n % step > int.fromInt(0) then step * ((n / step) + int.fromInt(1)) else n
    inline def prev(n: A, step: A) = if n % step > int.fromInt(0) then step * (n / step) else n

  given s_frac: Steppable[Double] with
    inline def next(n: Double, step: Double) = (n / step).ceil * step
    inline def prev(n: Double, step: Double) = (n / step).floor * step

  extension [A](n: A)(using s: Steppable[A])
    inline def next(step: A): A = s.next(n, step)
    inline def prev(step: A): A = s.prev(n, step)
end Math

transparent trait Util extends Math:
  implicit class SeqOps[A, S[B] <: Iterable[B]](seq: S[A]):
    def forindex(f: (A, Int) => Unit): S[A] =
      seq.zipWithIndex.foreach(f.tupled)
      seq

  def toColor(color: java.awt.Color): Color =
    Color.fromRGBA(color.getRed, color.getGreen, color.getBlue, color.getAlpha)

  case class Timed[A](value: A, instant: Instant)

  extension [A <: ObjectProxy](bank: Bank[A])
    def itemView: IndexedSeqView[A] = (0 until bank.itemCount().get()).view.map(bank.getItemAt)

    def fullView: IndexedSeqView[A] = (0 until bank.getCapacityOfBank()).view.map(bank.getItemAt)

object Util extends Util:
  val EIGHT: Vector[Int] = (0 to 7).toVector

  var println: String => Unit = null

  type SelfEqual[A] = CanEqual[A, A]

  /** Useful generic stuff like tracing and casting */
  extension [A](obj: A)
    transparent inline def trace(): A =
      Util.println(obj.toString)
      obj

    @targetName("tracem")
    transparent inline def trace(inline msg: String): A =
      if msg.nonEmpty then Util.println(s"$msg $obj")
      obj

    @targetName("tracef")
    transparent inline def trace(inline msg: A => String): A =
      val s = msg(obj)
      if s.nonEmpty then Util.println(msg(obj))
      obj

    @targetName("tracefm")
    transparent inline def trace(inline msg: String, inline f: A => String): A =
      val s = f(obj)
      if s.nonEmpty || msg.nonEmpty then Util.println(s"$msg ${f(obj)}")
      obj

    inline def safeCast[B]: Option[B] =
      obj match // https://github.com/lampepfl/dotty/issues/16016
        case b: B => Some(b)
        case _    => None

    inline def safeMap[B, C](inline f: B => C): Option[C] =
      obj match
        case b: B => Some(f(b))
        case _    => None
  end extension

  def printColor(c: Color): Unit =
    Util.println((c.getRed, c.getGreen, c.getBlue, c.getAlpha).toString())
    Vector(c.getRed, c.getGreen, c.getBlue, c.getAlpha).foreach { v =>
      val arr = ByteBuffer.allocate(4).putFloat(v.toFloat).array()
      Util.println(arr.toSeq.map(_ & 0xff).map(s => f"$s%02x").mkString(" "))
    }
  val rainbow   = Vector(RED, ORANGE, YELLOW, GREEN, LIME, CYAN, MAGENTA, FUCHSIA)
  val rainbow16 = (0 until 16).map(i => (i + 1) * 4).toVector

  def serialize[A](o: A): String =
    val bos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(o)
    oos.close()
    java.util.Base64.getEncoder().encodeToString(bos.toByteArray())

  def deserialize[A](s: String): Either[Throwable, A] =
    Try {
      val bis = new ByteArrayInputStream(java.util.Base64.getDecoder().decode(s))
      val ois = new ObjectInputStream(bis)
      val obj = ois.readObject()
      ois.close()
      obj.asInstanceOf[A]
    }.toEither

  def fillNull[A <: Product](value: A, default: A)(using m: Mirror.ProductOf[A]): A =
    given CanEqual[Any, Null] = CanEqual.derived
    val arr = value.productIterator
      .zip(default.productIterator)
      .map((a, b) => if a == null then b else a)
      .toArray
    m.fromProduct(Tuple.fromArray(arr))

  def comparator[A, B](a: A, b: A)(f: A => B): Boolean =
    given CanEqual[B, B] = CanEqual.derived
    f(a) == f(b)

  def delay(delay: Int, f: => Unit)(using ext: MonsterJamExt): Unit =
    ext.host.scheduleTask(() => f, delay)

  inline def popup(s: String)(using ext: MonsterJamExt): Unit =
    ext.host.showPopupNotification(s)

  private val timers: mutable.Map[String, Timer] = mutable.HashMap.empty[String, Timer]

  def wait(ms: Int, key: String, f: () => Unit): Unit =
    timers.get(key) match
      case None =>
        val timer = new Timer(
          ms,
          (_: ActionEvent) =>
            f()
            timers.remove(key)
        )
        timer.setRepeats(false)
        timers.update(key, timer)
      case Some(timer) => timer.restart()

  inline def timed[A](msg: String)(f: => A): A =
    val now    = Instant.now()
    val result = f
    val delta  = Duration.between(now, Instant.now()).toMillis()
    println(s"$msg: $delta ms")
    result
end Util
