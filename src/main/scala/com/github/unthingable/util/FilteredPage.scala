package com.github.unthingable.util

import com.bitwig.extension.controller.api.*
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
