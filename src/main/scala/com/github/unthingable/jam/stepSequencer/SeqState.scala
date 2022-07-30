package com.github.unthingable.jam.stepSequencer

import com.bitwig.extension.controller.api.NoteStep

enum StepMode(val keyRows: Int):
  case One extends StepMode(1)
  case Two extends StepMode(2)
  // case OneFull extends StepMode(1)
  case Four  extends StepMode(4)
  case Eight extends StepMode(8)

case class ViewPort(row1: Int, col1: Int, row2: Int, col2: Int):
  lazy val height = row2 - row1
  lazy val width  = col2 - col1
  lazy val size   = height * width

case class Point(x: Int, y: Int)

inline def Noop = () => Vector.empty

case class StepState(steps: List[(Point, NoteStep)], noRelease: Boolean)

case class SeqState(
  channel: Int,
  velocity: Int,
  stepSizeIdx: Int,
  stepMode: StepMode,
  stepScrollOffset: Int,
  keyScrollOffset: Int, // TOP of viewport
  noteVelVisible: Boolean,
) extends Serializable:

  lazy val stepViewPort = if noteVelVisible then ViewPort(0, 0, 4, 8) else ViewPort(0, 0, 8, 8)

  // how many notes are visible in the viewport
  lazy val keyPageSize: Int = (stepMode.keyRows / (8 / stepViewPort.height)).max(1)

  // how many steps are visible in the viewport (per note)
  inline def stepPageSize: Int = stepViewPort.size / keyPageSize

  inline def guardY(y: Int): Int = y.max(keyPageSize - 1.toInt).min(127)

  // resizing viewport can make offsets invalid
  lazy val keyScrollOffsetGuarded = guardY(keyScrollOffset)

  type StepFrac = (Int, Int) | Int
  val stepSizes = Vector[StepFrac](
    1 -> 64,
    1 -> 32,
    1 -> 16,
    1 -> 8,
    1 -> 4,
    1 -> 2,
    1,
    2,
    4,
    8
  )

  lazy val stepSize: Double = stepSizes(stepSizeIdx) match
    case (a, b) => a.toDouble / b.toDouble
    case x: Int => x

  lazy val stepString: String = stepSizes(stepSizeIdx) match
    case (a, b) => s"$a/$b"
    case x: Int => s"$x"

object SeqState:
  def empty: SeqState = SeqState(
    channel = 0,
    velocity = 100,
    stepSizeIdx = 5,
    stepMode = StepMode.One,
    stepScrollOffset = 0,
    keyScrollOffset = 12 * 3, // C1
    noteVelVisible = false,
  )
