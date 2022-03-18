package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{CursorRemoteControlsPage, Preferences, SettableBooleanValue, SettableEnumValue, Settings}
import com.github.unthingable.jam.surface.JamColor.JamColorBase.{CYAN, FUCHSIA, GREEN, LIME, MAGENTA, ORANGE, RED, YELLOW}

import java.awt.event.ActionEvent
import java.nio.ByteBuffer
import java.time.Instant

trait Util {
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

class Printer(printFun: String => Unit) {
  import javax.swing.Timer

  val timer = new Timer(100, (_: ActionEvent) => printFun(""))

  timer.setRepeats(false)

  def println(s: String): Unit = {
    if (timer.isRunning)
      timer.stop()
    printFun(s)
    timer.restart()
  }
}

object Util extends Util {
  var println: String => Unit = null
  //var ext: MonsterJamExt = null

  def printColor(c: Color): Unit = {
    Util.println((c.getRed, c.getGreen, c.getBlue, c.getAlpha).toString())
    Vector(c.getRed, c.getGreen, c.getBlue, c.getAlpha).foreach { v =>
      val arr = ByteBuffer.allocate(4).putFloat(v.toFloat).array()
      Util.println(arr.toSeq.map(_ & 0xff).map(s => f"$s%02x").mkString(" "))
    }
  }
  val rainbow = Vector(RED, ORANGE, YELLOW, GREEN, LIME, CYAN, MAGENTA, FUCHSIA)
}

case class FilteredPage(c: CursorRemoteControlsPage, f: String => Boolean) {
  c.pageNames().markInterested()
  c.selectedPageIndex().markInterested()

  def selectPrevious: () => Unit = () => prev.foreach(c.selectedPageIndex().set(_))

  def selectNext: () => Unit = () => next.foreach(c.selectedPageIndex().set(_))

  def hasNext: () => Boolean = () => next.isDefined

  def hasPrevious: () => Boolean = () => prev.isDefined

  def next: Option[Int] = {
    val current = c.selectedPageIndex().get()
    c.pageNames().get().zipWithIndex.find {case (name, idx) => idx > current && f(name)}.map(_._2)
  }

  def prev: Option[Int] = {
    val current = c.selectedPageIndex().get()
    c.pageNames().get().zipWithIndex.findLast {case (name, idx) => idx < current && f(name)}.map(_._2)
  }

}

object JamSettings {
  object ShowHide extends Enumeration {
    val Show, Hide = Value
  }

  object LimitLevels extends Enumeration {
    val None = Value
    val Zero = Value("0dB")
    val MinusTen = Value("-10dB")
    val Smart = Value
  }

  object DpadScroll extends Enumeration {
    val RegularOne = Value("single/page")
    val RegularPage = Value("page/single")
  }

  trait EnumSetting[A <: Enumeration] {
    def outerEnum: A
    def setting: SettableEnumValue
    def set(v: A#Value): Unit
    def get(): A#Value
    def addValueObserver(f: A#Value => Unit): Unit
  }

  def enumSetting[A <: Enumeration: ValueOf](p: Settings, s: String, category: String, init: A#Value) =
    new EnumSetting[A] {
      val outerEnum: A = valueOf[A]

      // side effect expected on creation
      val setting: SettableEnumValue = p.getEnumSetting(s, category, outerEnum.values.toArray.map(_.toString), init.toString)
      setting.markInterested()

      def set(v: A#Value): Unit = setting.set(v.toString)

      def get(): A#Value = outerEnum.withName(setting.get())

      def addValueObserver(f: A#Value => Unit): Unit = setting.addValueObserver(v => f(outerEnum.withName(v)))
    }
}
