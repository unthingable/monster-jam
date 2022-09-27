package com.github.unthingable.jam.stepSequencer

import com.github.unthingable.framework.quant
import com.bitwig.extension.controller.api.NoteStep
import java.time.Instant

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

case class PointStep(point: Point, step: NoteStep, pressed: Instant)

case class StepState(steps: List[PointStep], noRelease: Boolean)

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

  lazy val stepSize: Double = quant.stepSize(stepSizeIdx)

  lazy val stepString: String = quant.stepString(stepSizeIdx)

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
