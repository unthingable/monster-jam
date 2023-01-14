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

    // "scaled note" is the index into fullScale
    protected[state] lazy val fullScale: IndexedSeq[RealNote]         = (0 until 128).filter(isInScale(_))
    protected[state] lazy val fullScaleMap: Map[ScaledNote, RealNote] = fullScale.zipWithIndex.toMap
    protected[state] lazy val inverseMap: Map[RealNote, ScaledNote]   = fullScaleMap.map(_.swap)

    lazy val isChromatic: Boolean = intervals.size == 12

    lazy val length = fullScale.length

    def isInScale(note: RealNote): Boolean =
      intervals.contains((root + note) % 12)

    def nextInScale(note: RealNote, skip: Int = 0): Option[RealNote] =
      (note + skip until 128).view.find(isInScale(_))

    def prevInScale(note: RealNote, skip: Int = 0): Option[RealNote] =
      (note - skip).to(0, -1).view.find(isInScale(_))

    inline def notesRemainingUp(note: RealNote): Option[Int] =
      nextInScale(note).flatMap(inverseMap.get).map(fullScale.size - 1 - _)

    inline def notesRemainingDown(note: RealNote): Option[Int] =
      prevInScale(note).flatMap(inverseMap.get)

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

  case class ViewPort(rowTop: Int, colLeft: Int, rowBottom: Int, colRight: Int):
    lazy val height = rowBottom - rowTop
    lazy val width  = colRight - colLeft
    lazy val size   = height * width

  case class Point(x: Int, y: Int)

  inline def Noop = () => Vector.empty

  /**
    * A step on the grid.
    *
    * @param point Grid location of the step
    * @param _step is allowed to be by-name, created steps are not reported back immediately
    * @param pressed Time when step was pressed on the sequencer grid
    */
  class PointStep(val point: Point, _step: => NoteStep, val pressed: Instant):
    def step = _step
  object PointStep:
    def unapply(ps: PointStep) = Some((ps.point, ps.step, ps.pressed))

  case class StepState(steps: List[PointStep], noRelease: Boolean)

  enum ExpMode:
    case Exp, Operator

  case class SeqState(
    val channel: Int,
    val velocity: Int,
    val stepSizeIdx: Int,
    val stepMode: StepMode,
    val stepScrollOffset: Int,
    keyScaledOffset: ScaledNote, // BOTTOM of viewport
    val noteVelVisible: Boolean,
    val expMode: ExpMode,
    val scaleIdx: Int,
    val scaleRoot: RealNote // starts at 0
  ) extends Serializable:

    lazy val stepViewPort = if noteVelVisible then ViewPort(0, 0, 4, 8) else ViewPort(0, 0, 8, 8)

    // how many notes are visible in the viewport
    lazy val keyPageSize: Int = (stepMode.keyRows / (stepViewPort.width / stepViewPort.height)).max(1)

    lazy val scale = scales(scaleIdx)(scaleRoot)

    private def _isNoteVisible(note: ScaledNote): Boolean =
      note < keyScaledOffset + keyPageSize && note >= keyScaledOffset

    @targetName("isRealNoteVisible")
    inline def isNoteVisible(note: RealNote): Boolean = toScale(note).map(_isNoteVisible).getOrElse(false)

    @targetName("isScaledNoteVisible")
    inline def isNoteVisible(note: ScaledNote): Boolean = _isNoteVisible(note)

    // how many steps are visible in the viewport (per note)
    lazy val stepPageSize: Int = stepViewPort.size / keyPageSize

    // bound y by allowable range given the current window and scale
    inline def guardYReal(y: RealNote): RealNote = y.max(0).min(127 - keyPageSize)

    inline def guardYScaled(y: ScaledNote): ScaledNote = y.max(0).min(scale.fullScale.size - keyPageSize)

    inline def fromScale(y: ScaledNote): RealNote = scale.fullScale(y.max(0).min(scale.fullScale.size - 1))

    inline def toScale(y: RealNote): Option[ScaledNote] = scale.inverseMap.get(y.max(0).min(127))

    // resizing viewport can make offsets invalid
    lazy val keyScrollOffsetGuarded = guardYScaled(keyScaledOffset)

    lazy val stepSize: Double = quant.stepSize(stepSizeIdx)

    lazy val stepString: String = quant.stepString(stepSizeIdx)

  object SeqState:
    def empty: SeqState = SeqState(
      channel = 0,
      velocity = 100,
      stepSizeIdx = 5,
      stepMode = StepMode.Four,
      stepScrollOffset = 0,
      keyScaledOffset = 12 * 3, // C1
      noteVelVisible = false,
      expMode = ExpMode.Exp,
      scaleIdx = scales.length - 1, // chromatic
      scaleRoot = 0,
    )

    enum NoteName:
      case C, `C#`, D, `D#`, E, F, `F#`, G, `G#`, A, `A#`, B

    def toNoteName(note: RealNote): String =
      s"${NoteName.values(note % 12)}${note / 12 - 2}"

    def toNoteNameNoOct(note: RealNote): String =
      NoteName.values(note % 12).toString
