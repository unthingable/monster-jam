package com.github.unthingable.jam.surface

object BlackSysexMagic:
  val deviceId = "1500"

  def createCommand(commandId: String, value: String): String =
    "f0002109" + deviceId + "4d500001" + commandId + value + "f7"

  val ShiftDownCommand: String      = createCommand("4d", "01")
  val ShiftReleaseCommand: String   = createCommand("4d", "00")
  val ReturnFromHostCommand: String = createCommand("46", "01")

  val zeroStrips = "f000210915004d5000010500000000000000000000000000000000f7"

  case class BarMode(v: String) extends AnyVal derives CanEqual

  object BarMode:
    val DUAL   = BarMode("03")
    val DOT    = BarMode("01")
    val PAN    = BarMode("02")
    val SINGLE = BarMode("00")
end BlackSysexMagic
