package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.Track
import com.github.unthingable.Util
import com.github.unthingable.framework.mode.{CycleMode, ModeButtonCycleLayer}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamTouchStrip
import com.github.unthingable.jam.{Jam, SliderBankMode}

import scala.collection.mutable

trait Level { this: Jam =>
  lazy val levelCycle = new ModeButtonCycleLayer("LEVEL", j.level, CycleMode.Cycle) with Util {
    override val subModes = Vector(
      new SliderBankMode[Track]("strips volume", trackBank.getItemAt, _.volume()) {
        EIGHT.foreach { idx =>
          val track = trackBank.getItemAt(idx)
          track.trackType().markInterested()
          track.trackType().addValueObserver(v => if (isOn) updateLimits(Some(idx, v)))
        }
        ext.preferences.limitLevel.addValueObserver(_ => if (isOn) updateLimits(None))

        val paramLimits: mutable.Seq[Double] = mutable.ArrayBuffer.fill(8)(1.0)

        override val barMode: BarMode = BarMode.DUAL

        proxies.forindex { case (track, idx) =>
          val strip: JamTouchStrip = j.stripBank.strips(idx)
          track.addVuMeterObserver(128, -1, true, v => if (isOn) strip.update(v))
        }

        override def onActivate(): Unit = {
          super.onActivate()
          // clear meter values from whatever was happening before, let VU meters self update
          j.stripBank.strips.foreach(_.update(0))
          updateLimits(None)
        }

        override def paramRange(idx: Int): (Double, Double) = (0.0, paramLimits(idx))

        def updateLimits(maybeType: Option[(Int, String)], bind: Boolean = true): Unit = {
          val max      = 1.259921049894873
          val zero     = 1.0
          val minusTen = 0.6812920690579614

          maybeType match {
            case Some((idx, trackType)) =>
              import com.github.unthingable.JamSettings.LimitLevels
              paramLimits.update(idx, ext.preferences.limitLevel.get() match {
                case LimitLevels.None   => 1.0
                case LimitLevels.Smart  =>
                  trackType match {
                    case "Group" => zero / max
                    //case "Effect" | "Master" => 1.0
                    case _ => minusTen / max
                  }
                case LimitLevels.`0dB`   => zero / max
                case LimitLevels.`-10dB` => minusTen / max
                // case _        => 1.0
              })
              Util.println(f"updateLimits: $idx limit ${paramLimits(idx)}%1.2f:$trackType")
              if (bind)
                bindWithRange(idx)
            case None                   => EIGHT.map(idx => Some((idx, trackBank.getItemAt(idx).trackType().get()))).foreach(updateLimits(_, bind))
          }
        }
      },
      new SliderBankMode[Track]("strips pan", trackBank.getItemAt, _.pan()) {
        override val barMode: BarMode = BarMode.PAN
      },
    )
  }
}
