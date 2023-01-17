package com.github.unthingable.framework

import com.bitwig.extension.controller.api.BeatTimeValue
import com.github.unthingable.MonsterJamExt

package object quant:
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

  val stepFracMap: Map[String, StepFrac] = stepSizes.map(frac => (stepString(frac), frac)).toMap

  /* Step sizes for clip */
  val stepMap: Map[String, Double] = stepFracMap.mapValues(stepSize(_)).toMap

  inline def stepSize(frac: StepFrac): Double = frac match
    case (a, b) => a.toDouble / b.toDouble
    case x: Int => x

  inline def stepSize(stepSizeIdx: Int): Double = stepSize(stepSizes(stepSizeIdx))

  inline def stepString(frac: StepFrac): String = frac match
    case (a, b) => s"$a/$b"
    case x: Int => s"$x"

  inline def stepString(stepSizeIdx: Int): String = stepString(stepSizes(stepSizeIdx))

  /** Previous and next q grid points
    *
    * @param qString
    *   as reported by app settings, 1/16 to 8
    * @return
    *   current position wrapped by previous and next grid points as beattimevalue doubles (quarter notes)
    */
  def quantGrid(qString: String)(using ext: MonsterJamExt): Option[(Double, Double, Double)] =
    stepFracMap
      .get(qString)
      .map(f =>
        val now: Double = ext.transport.playPosition().get()
        val qLength: Double = f match
          // a fraction indicating a note
          case (_, b) => 4.0 / b
          // a whole bar, so we need to check time signature to calculate
          case x: Int =>
            val sig = ext.transport.timeSignature()
            x * 4.0 * sig.numerator().get() / sig.denominator().get()
        val completedDivisions: Int = (now / qLength).toInt
        (qLength * completedDivisions, now, qLength * (completedDivisions + 1))
      )

  /** How close we are to previous and next q grid points, in quarter notes */
  def gridDistance(qString: String)(using MonsterJamExt): Option[(Double, Double)] =
    quantGrid(qString).map((prev, now, next) => (now - prev, next - now))

  def gridDistanceWithNow(qString: String)(using MonsterJamExt): Option[(Double, Double, Double)] =
    quantGrid(qString).map((prev, now, next) => (now - prev, now, next - now))
end quant
