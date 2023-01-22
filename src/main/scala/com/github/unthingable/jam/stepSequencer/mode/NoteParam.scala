package com.github.unthingable.jam.stepSequencer.mode

import com.bitwig.extension.controller.api.Clip
import com.bitwig.extension.controller.api.NoteStep
import com.bitwig.extension.controller.api.NoteStep.State as NSState
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.framework.GetSetProxy
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.mode.CycleMode
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.ModeButtonCycleLayer
import com.github.unthingable.framework.mode.ModeCycleLayer
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.jam.JamParameter
import com.github.unthingable.jam.SliderBankMode
import com.github.unthingable.jam.SliderOp
import com.github.unthingable.jam.stepSequencer.StepCap
import com.github.unthingable.jam.stepSequencer.state.ExpMode
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamSurface

given Util.SelfEqual[NoteStep.State] = CanEqual.derived

trait NoteParam(using ext: MonsterJamExt, j: JamSurface) extends StepCap:
  object noteParam
      extends ModeCycleLayer(
        "stepNoteParam",
      ):

    override def modeBindings: Seq[Binding[?, ?, ?]] = Vector.empty

    case class FineStep(step: NoteStep):
      def offset: Int = step.x % fineRes
      def moveFineBy(clip: Clip, dx: Int): Unit =
        // ensure no crossings
        if step.x / 128 == (step.x + dx) / 128 then clip.moveStep(ts.channel, step.x, step.y, dx, 0)

    def toFine(step: NoteStep): Option[FineStep] =
      (0 until fineRes).view
        .map(i => fineClip.getStep(ts.channel, step.x * fineRes + i, step.y))
        .find(_.state() == NSState.NoteOn)
        .map(s => FineStep(s))

    import GetSetProxy.given
    val P = GetSetProxy[NoteStep, Double](0)

    val proxies: Vector[Option[GetSetProxy[NoteStep, Double]]] = Vector(
      // -- expressions
      P(_.velocity(), (s, v, _) => s.setVelocity(v)),
      P(_.releaseVelocity(), (s, v, _) => s.setReleaseVelocity(v)),
      P(_.velocitySpread(), (s, v, _) => s.setVelocitySpread(v)),
      // note start
      P(
        toFine(_).map(_.offset).getOrElse(0) / 128.0,
        (s, _, d) => toFine(s).foreach(_.moveFineBy(fineClip, (d * 128).toInt))
      ),
      P(_.duration(), (s, v, _) => s.setDuration(v)),
      P(_.pan(), (s, v, _) => s.setPan(v)),
      P(_.timbre(), (s, v, _) => s.setTimbre(v)),
      P(_.pressure(), (s, v, _) => s.setPressure(v)),
      // -- operators
      P(_.chance(), (s, v, _) => s.setChance(v)),
      // P(_.occurrence().ordinal() / NoteOccurrence.values().length.toDouble, (s, v) => s.setOccurrence(NoteOccurrence.values.apply((v * 10).toInt))),
      // P(_.isRecurrenceEnabled(), (s, v) => s.setChance(v)),
      // P(_.recurrenceLength() / 9.0, (s, v) =>
      //   val rec = (v * 9).toInt
      //   if (rec == 0)
      //     s.setIsRecurrenceEnabled(false)
      //   else
      //     s.setIsRecurrenceEnabled(true)
      //     s.setRecurrence
      //   ),
      (), // occurence
      (), // recurrence
      (), // recurrence
      P(_.repeatCount(), (s, v, _) => s.setChance(v)),
    ).map {
      case p: GetSetProxy[NoteStep, Double] @unchecked => Some(p)
      case _: Unit                                     => None
    }

    val realProxies = proxies.flatten
    val expProxies  = proxies.slice(0, 8)
    val opProxies   = proxies.slice(8, 16)
    def proxiesForState = ts.expMode match
      case ExpMode.Exp      => expProxies
      case ExpMode.Operator => opProxies

    def mask = proxiesForState.map(_.isDefined)

    def setCurrentSteps(): Unit =
      setCurrentSteps(localState.stepState.get.steps.view.map(_.step))

    def setCurrentSteps(steps: Iterable[NoteStep]): Unit =
      if steps.nonEmpty then
        Util.println(s"setting active $mask")
        realProxies.foreach(_.setTarget(steps))
        j.stripBank.setActive(mask)
        proxiesForState
          .zip(sliders.sliderOps)
          .foreach((p, s) => p.foreach(realp => s.set(realp.get, SliderOp.Source.Internal)))
      else
        realProxies.foreach(_.clearTarget())
        Util.println("setting inactive")
        sliders.sliderOps.foreach(_.set(0, SliderOp.Source.Internal))
        j.stripBank.setActive(_ => false)

    val callbacks: Vector[Option[Double => Unit]] =
      proxies.map(_.map(_.set))

    val sliders = new SliderBankMode(
      "noteExp",
      callbacks,
      _.map(JamParameter.Internal.apply).getOrElse(JamParameter.Empty),
      Seq.fill(8)(BarMode.SINGLE),
      stripColor = Some(Util.rainbow)
    ):
      override def onActivate(): Unit =
        super.onActivate()
        setCurrentSteps()

    override def onActivate(): Unit =
      super.onActivate()
      select(0)
    override val subModes: Vector[ModeLayer] = Vector(sliders)
  end noteParam
end NoteParam
