package com.github.unthingable.jam.surface

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{HardwareLightVisualState, InternalHardwareLightState}

object JamColor {
  object JamColorBase {
    val OFF = 0
    val RED = 4 //1
    val ORANGE = 8 //2
    val LIGHT_ORANGE = 12 //3
    val WARM_YELLOW = 16 //4
    val YELLOW = 20 //5
    val LIME = 24 //6
    val GREEN = 28 //7
    val MINT = 32 //8
    val CYAN = 36 //9
    val TURQUOISE = 40 //10
    val BLUE = 44 //11
    val PLUM = 48 //12
    val VIOLET = 52 //13
    val PURPLE = 56 //14
    val MAGENTA = 60 //15
    val FUCHSIA = 64 //16
    val WHITE = 68 //17

    inline def next(c: Int, inc: Int): Int =
      ((c / 4 + (inc - 1)) % 15 + 1) * 4 
  }
}

case class JamColorState(color: Int, brightness: Int) extends InternalHardwareLightState {
  override def getVisualState: HardwareLightVisualState = null
  val value: Int = if (brightness >= 0) color + brightness else 0
}

object JamColorState {
  val empty: JamColorState = JamColorState(0, 0)

  inline def apply(color: Color, brightness: Int): JamColorState = JamColorState(toColorIndex(color), brightness)

  def toColorIndex(color: Color): Int =
    NIColorUtil.convertColor(color.getRed.toFloat, color.getGreen.toFloat, color.getBlue.toFloat)
}
