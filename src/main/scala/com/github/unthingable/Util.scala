package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.CursorRemoteControlsPage
import com.github.unthingable.jam.surface.JamColor.JAMColorBase.{CYAN, FUCHSIA, GREEN, LIME, MAGENTA, ORANGE, RED, YELLOW}

import java.awt.event.ActionEvent
import java.nio.ByteBuffer

trait Util {
  implicit class SeqOps[A, S[B] <: Iterable[B]](seq: S[A]) {
    def forindex(f: (A, Int) => Unit): S[A] = {
      seq.zipWithIndex.foreach(f.tupled)
      seq
    }
  }

  def toColor(color: java.awt.Color): Color =
    Color.fromRGBA(color.getRed, color.getGreen, color.getBlue, color.getAlpha)
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

object Util {
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

object ShowHide extends Enumeration {
  type ShowHide = Value
  val Show, Hide = Value
}
