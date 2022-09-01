package com.github.unthingable.framework

package object quant {
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

  val stepMap: Vector[(String, Double)] = stepSizes.map(frac => (stepString(frac), stepSize(frac)))

  inline def stepSize(frac: StepFrac): Double = frac match
    case (a, b) => a.toDouble / b.toDouble
    case x: Int => x

  inline def stepSize(stepSizeIdx: Int): Double = stepSize(stepSizes(stepSizeIdx))

  inline def stepString(frac: StepFrac): String = frac match
    case (a, b) => s"$a/$b"
    case x: Int => s"$x"

  inline def stepString(stepSizeIdx: Int): String = stepString(stepSizes(stepSizeIdx))
}
