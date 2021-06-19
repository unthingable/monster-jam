package com.github.unthingable

import com.bitwig.extension.api.Color
import com.bitwig.extension.callback.BooleanValueChangedCallback
import com.bitwig.extension.controller.api.{BooleanValue, ControllerHost, Cursor, CursorRemoteControlsPage, HardwareActionBindable, RelativeHardwareControl, RelativeHardwareControlBinding}

trait Util {
  implicit class SeqOps[A, S[B] <: Seq[B]](seq: S[A]) {
    def forindex(f: (A, Int) => Unit): S[A] = {
      seq.zipWithIndex.foreach(f.tupled)
      seq
    }
  }

  def toColor(color: java.awt.Color): Color =
    Color.fromRGBA(color.getRed, color.getGreen, color.getBlue, color.getAlpha)
}

object Util {
  var println: String => Unit = null
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
