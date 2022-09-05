package com.github.unthingable.jam

import com.bitwig.extension.callback.DoubleValueChangedCallback
import com.bitwig.extension.controller.api.{
  Channel,
  Device,
  ObjectProxy,
  Parameter,
  RemoteControl,
  Send
}
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.framework.binding.{Binding, BindingBehavior => BB, HB}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.{JamColorState, JamSurface, JamTouchStrip, NIColorUtil}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class SliderBankMode[P <: ObjectProxy](
  override val id: String,
  val obj: Int => P,
  val param: P => Parameter,
  val stripColor: Option[Int => Int] = None
)(using ext: MonsterJamExt, j: JamSurface)
    extends SimpleModeLayer(id)
    with Util {

  import SliderBankMode._

  val proxies: Vector[P]                     = j.stripBank.strips.indices.map(obj).toVector
  val sliderParams: Vector[Parameter]        = proxies.map(param)
  val paramState: mutable.ArrayBuffer[State] = mutable.ArrayBuffer.fill(8)(State.Normal)

  proxies.zip(sliderParams).foreach((proxy, param) =>
    param.markInterested()
    param.name().markInterested()
    proxy.exists().markInterested()    
  )

  val barMode: Seq[BarMode]
  val paramKnowsValue: Boolean = true // UserControls don't and that's sad
  val paramValueCache: ArrayBuffer[Double] = mutable.ArrayBuffer.fill(8)(0.0) // unscaled

  def paramValue(idx: Int): Double =
    if (paramKnowsValue) sliderParams(idx).value().get() else paramValueCache(idx)
  def paramRange(idx: Int): (Double, Double) = (0.0, 1.0)

  def bindWithRange(idx: Int, force: Boolean = false): Unit =
    if (force || paramState(idx) == State.Normal) { // check state when binding from outside
      val (min, max) = paramRange(idx)
      // ext.host.println(s"$name binding $idx with $max")
      j.stripBank.strips(idx).slider.setBindingWithRange(sliderParams(idx), min, max)
      // force update to account for value lag when scrolling bank
      stripObserver(idx).valueChanged(sliderParams(idx).value().get())
    }

  def stripObserver(idx: Int): DoubleValueChangedCallback =
    (v: Double) =>
      if (isOn && !j.clear.btn.isPressed().get) {
        j.stripBank.setValue(idx, (1.0.min(v / paramRange(idx)._2) * 127).toInt)
        paramValueCache.update(idx, v)
      }

  override def modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.indices.flatMap { idx =>
    val strip: JamTouchStrip = j.stripBank.strips(idx)
    val proxy: ObjectProxy   = proxies(idx)
    val param: Parameter     = sliderParams(idx)

    var offsetObserver: Double => Unit = _ => ()
    strip.slider.value().addValueObserver(offsetObserver(_))

    var startValue: Option[Double] = None

    def engage(event: Event): Unit = {
      import Event._
      import State._

      val shiftOn = j.Mod.Shift.btn.isPressed
      val stripOn = strip.slider.isBeingTouched.get()

      val state = (shiftOn, stripOn, event, paramState(idx)) match {
        case (_, _, ClearP, _) =>
          strip.slider.clearBindings()
          Normal
        case (_, _, ClearR, _) =>
          bindWithRange(idx, force = true)
          Normal
        case (_, _, StripP, _) if j.clear.btn.isPressed().get =>
          param.reset()
          Normal
        case (_, _, ShiftP, _) =>
          strip.slider.clearBindings()
          ShiftTracking
        case (true, true, _: PressEvent, state) =>
          if (state == Normal)
            strip.slider.clearBindings()
          val current = param.get()
          startValue = None
          offsetObserver = { v =>
            val offset  = (v - startValue.getOrElse(v)) * 0.2
            val floored = (current + offset).max(0).min(1)
            param.set(floored)
            if (startValue.isEmpty) startValue = Some(v)
          }
          ShiftTracking
        case (true, _, StripR, ShiftTracking) =>
          offsetObserver = _ => ()
          stripObserver(idx).valueChanged(param.value().get())
          ShiftTracking
        case (_, _, _: ReleaseEvent, ShiftTracking) =>
          offsetObserver = _ => ()
          bindWithRange(idx, force = true)
          stripObserver(idx).valueChanged(param.value().get())
          Normal
        case _ =>
          Normal
      }
      paramState.update(idx, state)
    }

    proxy.exists().addValueObserver(v => if (isOn) j.stripBank.setActive(idx, v))

    // move slider dot
    if (paramKnowsValue) {
      param.value().markInterested()
      param.value().addValueObserver(stripObserver(idx))
    } else {
      val tv = strip.slider.targetValue()
      val hv = strip.slider.hasTargetValue
      tv.markInterested()
      hv.markInterested()
      tv.addValueObserver(v => if (hv.get()) stripObserver(idx).valueChanged(v))
      hv.addValueObserver(v => stripObserver(idx).valueChanged(if (v) tv.get() else 0))
    }

    proxy match {
      case channel: Channel =>
        channel.color().markInterested()
        channel
          .color()
          .addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b))
          )
      case send: Send =>
        send.sendChannelColor().markInterested()
        send
          .sendChannelColor()
          .addValueObserver((r, g, b) =>
            if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b))
          )
      case _: RemoteControl =>
      case _: Parameter     => ()
      case _: Device        => ()
    }

    import Event._
    if (paramKnowsValue)
      Vector(
        HB(
          j.clear.btn.pressedAction,
          s"clear $idx pressed",
          () => engage(ClearP),
          BB(tracked = false, exclusive = false)
        ),
        HB(
          j.clear.btn.releasedAction,
          s"clear $idx released",
          () => engage(ClearR),
          BB(tracked = false, exclusive = false)
        ),
        HB(
          j.Mod.Shift.btn.pressedAction,
          s"shift $idx pressed",
          () => engage(ShiftP),
          BB(tracked = false, exclusive = false)
        ),
        HB(
          j.Mod.Shift.btn.releasedAction,
          s"shift $idx released",
          () => engage(ShiftR),
          BB(tracked = false, exclusive = false)
        ),
        HB(
          strip.slider.beginTouchAction,
          s"strip $idx pressed",
          () => engage(StripP),
          BB(tracked = true, exclusive = false)
        ),
        HB(
          strip.slider.endTouchAction,
          s"strip $idx released",
          () => engage(StripR),
          BB(tracked = true, exclusive = false)
        ),
      )
    else Vector( // for dirty tracking
      HB(
        strip.slider.beginTouchAction,
        s"strip $idx pressed",
        () => (),
        BB(tracked = true, exclusive = false)
      ),
      HB(
        strip.slider.endTouchAction,
        s"strip $idx released",
        () => (),
        BB(tracked = true, exclusive = false)
      ),
    )
  }

  private def sync(idx: Int, flush: Boolean = true): Unit =
    j.stripBank.setValue(idx, (paramValue(idx) * 127 / paramRange(idx)._2).toInt, flush)

  override def onActivate(): Unit = {

    // ext.host.println(barMode.toString)
    // ext.host.println(sliderParams.map(_.name().get()).mkString(","))
    // ext.host.println(sliderParams.map(_.value().get()).mkString(","))

    j.stripBank.barMode = barMode

    j.stripBank.strips.forindex {
      case (strip, idx) =>
        val proxy = proxies(idx)

        stripColor
          .map(_(idx))
          .orElse(proxy match {
            case channel: Channel =>
              Some(JamColorState.toColorIndex(channel.color().get()))
            case send: Send =>
              Some(JamColorState.toColorIndex(send.sendChannelColor().get()))
            case _: RemoteControl =>
              Some(Util.rainbow(idx))
            case _: Parameter =>
              // a random parameter we know nothing about (probably from UserControlBank)
              Some(JamColorBase.RED)
          })
          .foreach(c => j.stripBank.setColor(idx, c))

        j.stripBank.setActive(idx, value = proxy.exists().get, flush = false)
    }

    j.stripBank.flushColors()

    sliderParams.indices.foreach(sync(_, false))
    if (barMode.contains(BarMode.DUAL))
      j.stripBank.flushValues()

    j.stripBank.strips.indices.foreach(bindWithRange(_))
    super.onActivate()
  }

  override def onDeactivate(): Unit = {
    super.onDeactivate()
    j.stripBank.strips.foreach(_.slider.clearBindings())
  }
}

object SliderBankMode {
  sealed trait Event
  sealed trait ShiftEvent   extends Event
  sealed trait StripEvent   extends Event
  sealed trait PressEvent   extends Event
  sealed trait ReleaseEvent extends Event
  object Event {
    case object ShiftP extends ShiftEvent with PressEvent
    case object ShiftR extends ShiftEvent with ReleaseEvent
    case object StripP extends StripEvent with PressEvent
    case object StripR extends StripEvent with ReleaseEvent
    case object ClearP extends StripEvent with PressEvent
    case object ClearR extends StripEvent with ReleaseEvent
  }

  sealed trait State
  object State {
    case object ShiftTracking extends State
    case object Normal        extends State
  }

  sealed trait Value[A] { val value: A }
  object Value {
    case class Scaled[A](value: A)   extends Value[A]
    case class Unscaled[A](value: A) extends Value[A]
  }
}
