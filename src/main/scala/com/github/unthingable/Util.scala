package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{CursorRemoteControlsPage, Preferences, SettableBooleanValue, SettableEnumValue, Settings}
import com.github.unthingable.jam.surface.JamColor.JamColorBase.{CYAN, FUCHSIA, GREEN, LIME, MAGENTA, ORANGE, RED, YELLOW}

import java.awt.event.ActionEvent
import java.nio.ByteBuffer
import java.time.Instant

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scala.util.Try
import java.nio.charset.StandardCharsets
import scala.annotation.targetName

transparent trait Util {
  implicit class SeqOps[A, S[B] <: Iterable[B]](seq: S[A]) {
    def forindex(f: (A, Int) => Unit): S[A] = {
      seq.zipWithIndex.foreach(f.tupled)
      seq
    }
  }

  def toColor(color: java.awt.Color): Color =
    Color.fromRGBA(color.getRed, color.getGreen, color.getBlue, color.getAlpha)

  case class Timed[A](value: A, instant: Instant)
}
object Util extends Util {
  var println: String => Unit = null
  
  extension [A](obj: A)
    inline def trace(): A =
      Util.println(obj.toString)
      obj

    @targetName("tracem")
    inline def trace(msg: String): A =
      Util.println(s"$msg $obj")
      obj
    
    @targetName("tracefm")
    inline def trace(msg: A => String): A =
      Util.println(msg(obj))
      obj

  def printColor(c: Color): Unit = {
    Util.println((c.getRed, c.getGreen, c.getBlue, c.getAlpha).toString())
    Vector(c.getRed, c.getGreen, c.getBlue, c.getAlpha).foreach { v =>
      val arr = ByteBuffer.allocate(4).putFloat(v.toFloat).array()
      Util.println(arr.toSeq.map(_ & 0xff).map(s => f"$s%02x").mkString(" "))
    }
  }
  val rainbow = Vector(RED, ORANGE, YELLOW, GREEN, LIME, CYAN, MAGENTA, FUCHSIA)

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
}
