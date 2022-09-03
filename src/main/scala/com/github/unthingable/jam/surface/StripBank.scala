package com.github.unthingable.jam.surface

import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface.BlackSysexMagic.{BarMode, createCommand}

import scala.collection.mutable

class StripBank()(implicit ext: MonsterJamExt) extends Util {
  val strips: Vector[JamTouchStrip] = ('A' to 'H').map { idx =>
    new JamTouchStrip(
      touch = ext.xmlMap.button(s"CapTst$idx", ext.xmlMap.touchElems),
      slide = ext.xmlMap.knob(s"Tst$idx", ext.xmlMap.touchElems),
      led = ext.xmlMap.led(s"Tst${idx}IDX", ext.xmlMap.touchElems))

  }.toVector
    .forindex(_.slider.setIndexInGroup(_))

  var barMode: Seq[BarMode] = Seq.fill(8)(BarMode.DUAL)
  private val colors: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val values: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(8)(0)
  private val active: mutable.ArraySeq[Boolean] = mutable.ArraySeq.fill(8)(false)

  def setColor(idx: Int, color: Int, flush: Boolean = true): Unit = {
    // some more NI magic for you, white is not the same for strips. Also we can't really show black tracks.
    colors.update(idx, if (color == 68 || color == 0) 120 else color)
    if (flush) flushColors()
  }
  def setValue(idx: Int, value: Int, flush: Boolean = true): Unit = {
    if (barMode(idx) == BarMode.DUAL) {
      values.update(idx, value)
      if (flush) flushValues()
    } else strips(idx).update(value)
  }
  def setActive(idx: Int, value: Boolean, flush: Boolean = true): Unit = {
    active.update(idx, value)
    if (flush) flushColors()
  }

  def flushColors(): Unit =
    ext.midiOut.sendSysex(createCommand("05",
      colors.zip(active).zipWithIndex.map { case ((n, a), idx) => f"${if (a) barMode(idx).v else "00"}${n}%02x"}.mkString))

  def flushValues(): Unit = {
    ext.midiOut.sendSysex(createCommand("04", values.map(n => f"${n}%02x").mkString))
  }

  def clear(): Unit = ext.midiOut.sendSysex(BlackSysexMagic.zeroStrips)
}
