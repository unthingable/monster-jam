package com.github.unthingable.jam.stepSequencer

import com.github.unthingable.framework.quant
import com.bitwig.extension.controller.api.NoteStep
import java.time.Instant
import scala.annotation.targetName

object state:
  opaque type ScaledNote           = Int // note number in scale, scaled notes are always consecutive
  opaque type RealNote             = Int // what a clip expects
  opaque type Interval <: RealNote = Int
  extension (n: RealNote) def value: Int                = n
  extension (s: Seq[RealNote]) def apply(i: ScaledNote) = s(i)

  case class Scale(name: String, intervals: Set[Interval])(
    root: RealNote
  ): // notes in intervals from root, always 8 except chromatic
    lazy val fullScale: IndexedSeq[RealNote]         = (0 until 128).filter(isInScale(_))
    lazy val fullScaleMap: Map[ScaledNote, RealNote] = fullScale.zipWithIndex.toMap

    inline def isInScale(inline note: RealNote): Boolean =
      intervals.contains((root + note) % 12)

    // inline def nextInScale(inline note: Int): Option[Int] =
    //   (note until 128).view.find(isInScale(_))

    // inline def prevInScale(inline note: Int): Option[Int] =
    //   (note.to(0, -1)).view.find(isInScale(_))

  val scales: Vector[RealNote => Scale] = Vector(
    "Natural Minor"             -> "101101011010",
    "Major"                     -> "101011010101",
    "Dorian"                    -> "101101010110",
    "Phrygian"                  -> "110101011010",
    "Mixolydian"                -> "101011010110",
    "Melodic Minor (ascending)" -> "101101010101",
    "Harmonic Minor"            -> "101101011001",
    "Bebop Dorian"              -> "100111010110",
    "Blues"                     -> "100101110010",
    "Minor Pentatonic"          -> "100101010010",
    "Hungarian Minor"           -> "101100111001",
    "Ukranian Dorian"           -> "101100110110",
    "Marva"                     -> "110010110101",
    "Todi"                      -> "110100111001",
    "Whole Tone"                -> "101010101010",
    "Chromatic"                 -> "111111111111",
  ).map((name, seq) => Scale(name, seq.toCharArray.zipWithIndex.filter(_._1 == '1').map(_._2).toSet))

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

  enum ExpMode:
    case Exp, Operator

  case class SeqState(
    channel: Int,
    velocity: Int,
    stepSizeIdx: Int,
    stepMode: StepMode,
    stepScrollOffset: Int,
    keyScrollOffset: RealNote, // BOTTOM of viewport
    noteVelVisible: Boolean,
    expMode: ExpMode,
    scale: Scale,
    scaleRoot: RealNote
  ) extends Serializable:

    lazy val stepViewPort = if noteVelVisible then ViewPort(0, 0, 4, 8) else ViewPort(0, 0, 8, 8)

    // how many notes are visible in the viewport
    lazy val keyPageSize: Int = (stepMode.keyRows / (8 / stepViewPort.height)).max(1)

    private def _isNoteVisible(note: Int): Boolean =
      note < keyScrollOffset + keyPageSize && note >= keyScrollOffset

    @targetName("isRealNoteVisible")
    inline def isNoteVisible(note: RealNote): Boolean = _isNoteVisible(note)

    @targetName("isScaledNoteVisible")
    inline def isNoteVisible(note: ScaledNote): Boolean = _isNoteVisible(scale.fullScale(note))

    // how many steps are visible in the viewport (per note)
    inline def stepPageSize: Int = stepViewPort.size / keyPageSize

    // bound y by allowable range given the current window and scale
    inline def guardY(y: RealNote): RealNote = y.max(0).min(scale.fullScale.last.value - keyPageSize - 1)

    // resizing viewport can make offsets invalid
    lazy val keyScrollOffsetGuarded = guardY(keyScrollOffset)

    lazy val stepSize: Double = quant.stepSize(stepSizeIdx)

    lazy val stepString: String = quant.stepString(stepSizeIdx)

  object SeqState:
    def empty: SeqState = SeqState(
      channel = 0,
      velocity = 100,
      stepSizeIdx = 5,
      stepMode = StepMode.Four,
      stepScrollOffset = 0,
      keyScrollOffset = 12 * 3, // C1
      noteVelVisible = false,
      expMode = ExpMode.Exp,
      scale = scales.last(0.asInstanceOf[RealNote]), // chromatic
      scaleRoot = 0,
    )

    enum NoteName:
      case C, `C#`, D, `D#`, E, F, `F#`, G, `G#`, A, `A#`, B

    def toNoteName(note: RealNote): String =
      s"${NoteName.values(note % 12)}${note / 12 - 2}"
