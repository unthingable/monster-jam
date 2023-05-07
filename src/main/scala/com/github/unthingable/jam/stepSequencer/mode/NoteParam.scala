package com.github.unthingable.jam.stepSequencer.mode

import com.bitwig.extension.controller.api.Clip
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State as NSState
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.GetSetContainer
import com.github.unthingable.framework.GetSetProxy
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.MultiModeLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.JamParameter
import com.github.unthingable.jam.SliderBankMode
import com.github.unthingable.jam.SliderOp
import com.github.unthingable.jam.stepSequencer.StepCap
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamSurface

import scala.collection.mutable

import Util.{popup, trace, next}

given Util.SelfEqual[NoteStep.State] = CanEqual.derived

enum StepParam derives CanEqual:
  case Vel, RelVel, Spread, Nudge, Duration, Pan, Timbre, Pressure

trait NoteParam(using ext: MonsterJamExt, j: JamSurface) extends StepCap:
  object noteParam extends MultiModeLayer:

    override def id: String = "noteParam"

    override def modeBindings: Seq[Binding[?, ?, ?]] = Vector.empty

    case class FineStep(fineStep: NoteStep):
      def offset: Int = fineStep.x % fineRes

      def moveFineTo(fineClip: Clip, offset: Int): Unit =
        val x: Int          = fineStep.x
        val lowerBound: Int = (x / fineRes) * fineRes
        val newx: Int       = lowerBound + offset
        val dx: Int         = (newx - x).max(lowerBound - x).min(lowerBound + fineRes - 1 - x)

        // bracket duration
        val fineStepSize: Double = ts.stepSize / fineRes
        val fineDuration: Double = fineStep.duration / fineStepSize
        val currentEnd: Int      = x + fineDuration.toInt
        val newEnd: Int          = newx + fineDuration.toInt
        if dx > 0 then
          // next note in fine steps, if we're about to clobber it
          val nextNote: Option[Int] = (currentEnd to newEnd).find(
            fineClip.getStep(ts.channel, _, fineStep.y).state() == NSState.NoteOn
          )
          val nextStep: Int = currentEnd.next(fineRes)
          // correction for a limit past which we cannot extend, in fine steps
          val clobberOffset: Option[Int] =
            if ext.preferences.stepKeepEnd.get && newEnd > nextStep then
              nextNote.map(_.min(nextStep)).orElse(Some(nextStep))
            else nextNote
          clobberOffset match
            case None => ()
            case Some(value) =>
              val newDuration = fineStep.duration - ((newEnd - value) * fineStepSize)
              fineStep.setDuration(newDuration)
        end if

        fineClip.moveStep(ts.channel, x, fineStep.y, dx, 0)
      end moveFineTo
    end FineStep

    def toFine(step: NoteStep): Option[FineStep] = (0 until fineRes).view
      .map(i => fineClip.getStep(ts.channel, step.x * fineRes + i, step.y))
      .find(_.state() == NSState.NoteOn)
      .map(s => FineStep(s))

    /** Treat duration as length of last grid step occupied by note */
    extension (step: NoteStep)
      private inline def stepBounds: (Double, Double, Double) =
        val duration               = step.duration()
        val scaledDuration: Double = duration / ts.stepSize
        val complete               = scaledDuration.floor
        val incomplete             = scaledDuration.ceil
        (complete, incomplete, scaledDuration)

      inline def tail: Double =
        val (complete, incomplete, scaledDuration) = stepBounds
        if complete == incomplete then 1.0
        else scaledDuration - complete

      /** Calculate new duration given tail */
      inline def fromTail(newTail: Double): Double =
        val (complete, incomplete, _) = stepBounds
        ( // if complete == 0.0 then newTail
          // else
          if complete == incomplete then complete - 1 + newTail
          else complete + newTail
        ) * ts.stepSize

      inline def setTail(newTail: Double): Unit =
        step.setDuration(fromTail(newTail.max(0.001)))
    end extension

    import GetSetContainer.given
    val P = GetSetContainer[NoteStep, Double](0, _, _)

    val proxyList: Vector[(StepParam, GetSetContainer[NoteStep, Double])] = Vector(
      StepParam.Vel    -> P(_.velocity(), (s, v, _) => s.setVelocity(v)),
      StepParam.RelVel -> P(_.releaseVelocity(), (s, v, _) => s.setReleaseVelocity(v)),
      StepParam.Spread -> P(_.velocitySpread(), (s, v, _) => s.setVelocitySpread(v)),
      // note start
      StepParam.Nudge ->
        P(
          toFine(_).map(_.offset).getOrElse(0) / 128.0,
          (s, v, _) =>
            toFine(s).foreach(_.moveFineTo(fineClip, (v * 128).toInt))
            // Util.wait(20, "repeat setCurrent", () => setCurrentSteps()) // update duration slider
        ),
      StepParam.Duration -> P(_.tail, (s, v, _) => s.setTail(v)),
      StepParam.Pan      -> P(_.pan(), (s, v, _) => s.setPan(v)),
      StepParam.Timbre   -> P(_.timbre(), (s, v, _) => s.setTimbre(v)),
      StepParam.Pressure -> P(_.pressure(), (s, v, _) => s.setPressure(v)),
    )

    val proxyListNotify: Vector[(StepParam, GetSetContainer[NoteStep, Double])] = proxyList.map((param, getset) =>
      (
        param,
        P(
          getset.getf,
          (s, v, d) =>
            getset.setf(s, v, d)
            // will fire for each selected step, a little heavy but ok
            popup(f"$param: $v%.2f")
        )
      )
    )

    val proxyMap = proxyList.toMap

    // val proxiesXX = Vector(
    //   // -- expressions
    //   P(_.velocity(), (s, v, _) => s.setVelocity(v)),
    //   P(_.releaseVelocity(), (s, v, _) => s.setReleaseVelocity(v)),
    //   P(_.velocitySpread(), (s, v, _) => s.setVelocitySpread(v)),
    //   // note start
    //   P(
    //     toFine(_).map(_.offset).getOrElse(0) / 128.0,
    //     (s, _, d) => toFine(s).foreach(_.moveFineBy(fineClip, (d * 128).toInt))
    //   ),
    //   P(_.duration(), (s, v, _) => s.setDuration(v)),
    //   P(_.pan(), (s, v, _) => s.setPan(v)),
    //   P(_.timbre(), (s, v, _) => s.setTimbre(v)),
    //   P(_.pressure(), (s, v, _) => s.setPressure(v)),
    //   // -- operators
    //   P(_.chance(), (s, v, _) => s.setChance(v)),
    //   // P(_.occurrence().ordinal() / NoteOccurrence.values().length.toDouble, (s, v) => s.setOccurrence(NoteOccurrence.values.apply((v * 10).toInt))),
    //   // P(_.isRecurrenceEnabled(), (s, v) => s.setChance(v)),
    //   // P(_.recurrenceLength() / 9.0, (s, v) =>
    //   //   val rec = (v * 9).toInt
    //   //   if (rec == 0)
    //   //     s.setIsRecurrenceEnabled(false)
    //   //   else
    //   //     s.setIsRecurrenceEnabled(true)
    //   //     s.setRecurrence
    //   //   ),
    //   (), // occurence
    //   (), // recurrence
    //   (), // recurrence
    //   P(_.repeatCount(), (s, v, _) => s.setChance(v)),
    // )
    // // .map {
    // //   case p: GetSetProxy[NoteStep, Double] @unchecked => Some(p)
    // //   case _: Unit                                     => None
    // // }

    val proxies = Vector.fill(8)(GetSetProxy[NoteStep, Double](GetSetContainer.empty(0.0)))

    // def mask: Vector[Boolean] = proxies.map(_.isDefined)

    /** Force update sliderOp value from proxy */
    private def pushProxies(): Unit =
      proxies
        .zip(sliders.sliderOps)
        .foreach((p, s) => s.set(p.get, SliderOp.Source.Internal))
      // j.stripBank.flushValues()

    /** (Re)configure sliders when selected steps change */
    def setCurrentSteps(): Unit =
      val currentSteps = localState.stepState.get.steps.view

      // activation must have happened immediately prior
      if sliders.isOn then
        ts.stepParam match
          case None =>
            rowSelected = None

            // set sliders to one parameter each, consecutive from proxyList
            proxyListNotify.map(_._2).zip(proxies).foreach((getset, p) => p.update(getset))
            // target them all at current steps
            proxies.foreach(_.setTarget(currentSteps.map(_.step)))

            pushProxies()

            // low level interface with slider bank, but seems easier than the alternative
            for (color, idx) <- Util.rainbow16.slice(0, 8).zipWithIndex do
              j.stripBank.setColor(idx, color, flush = false)
              j.stripBank.setActive(idx, true, flush = false)

          case Some(paramKey) => // we're in single-row mode
            val lastStep = currentSteps.last
            rowSelected = Some(lastStep.point.y)

            // set all sliders to one specific parameter
            val paramProxy = proxyMap(paramKey)
            proxies.map(_.update(paramProxy))
            // target each at one of the 8 steps in a row
            for col <- 0 until 8 do
              val xy   = m2clip(lastStep.point.x, col).get
              val step = stepAt(xy._1, xy._2)

              j.stripBank.setColor(col, Util.rainbow16(paramKey.ordinal), flush = false)

              step.state() match
                case NSState.NoteOn =>
                  proxies(col).setTarget(Vector(step))
                  // slider value must update before setting active
                  sliders.sliderOps(col).set(proxies(col).get, SliderOp.Source.Internal)
                  j.stripBank.setActive(col, true, flush = false)
                case NSState.Empty | NSState.NoteSustain =>
                  proxies(col).clearTarget()
                  sliders.sliderOps(col).set(0, SliderOp.Source.Internal)
                  j.stripBank.setActive(col, false, flush = false)

        end match
        j.stripBank.flushColors()
      end if

    end setCurrentSteps

    // what sets the actual parameters in SliderBankMode
    val callbacks: Vector[Option[Double => Unit]] =
      proxies.map(p => Some(p.set))

    val colors = mutable.ArraySeq.from(Util.rainbow16.slice(0, 8))

    val sliders = new SliderBankMode(
      "noteExp",
      callbacks,
      _.map(JamParameter.Internal.apply).getOrElse(JamParameter.Empty),
      Seq.fill(8)(BarMode.SINGLE),
      stripColor = Some(Vector.fill(8)(1))
    ):
      override def onActivate(): Unit =
        // zero out to reduce flashing (setCurrentSteps will update)
          (0 until 8).foreach(sliderOps(_).set(0, SliderOp.Source.Internal))
          super.onActivate()

    override val subModes: Vector[ModeLayer]           = Vector(sliders, noteParamGate)
    override val subModesToActivate: Vector[ModeLayer] = subModes
  end noteParam

  object noteParamGate extends SimpleModeLayer("noteParamGate"):
    def toggle(idx: Int): Unit =
      val param = StepParam.fromOrdinal(idx)
      val newv = if ts.stepParam.contains(param) then
        popup("Note params: all")
        None
      else
        popup(s"Note params: row $param")
        Some(param)
      setState(ts.copy(stepParam = newv))
      noteParam.setCurrentSteps()

    override def modeBindings: Seq[Binding[?, ?, ?]] = (0 until 8)
      .map(idx =>
        val btn   = j.groupButtons(idx)
        val param = StepParam.fromOrdinal(idx)
        Vector(
          EB(btn.st.press, "", () => toggle(idx)),
          SupColorStateB(
            btn.light,
            () =>
              if ts.stepParam.contains(param) then JamColorState(JamColorBase.WHITE, 2)
              else JamColorState(Util.rainbow16(idx), 2)
          )
        )
      )
      .flatten
  end noteParamGate

end NoteParam
