package com.github.unthingable.jam

import com.bitwig.extension.callback.DoubleValueChangedCallback
import com.bitwig.extension.controller.api.{Channel, ObjectProxy, Parameter, RemoteControl, Send}
import com.github.unthingable.{MonsterJamExt, Util}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.{JamColorState, JamSurface, JamTouchStrip, NIColorUtil}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class SliderBankMode[P <: ObjectProxy](override val name: String, val obj: Int => P, val param: P => Parameter)
  (implicit ext: MonsterJamExt, j: JamSurface)
  extends SubModeLayer(name) with Util {

  import JAMColorBase._
  import SliderBankMode._

  val rainbow = Vector(RED, ORANGE, YELLOW, GREEN, LIME, CYAN, MAGENTA, FUCHSIA)

  val proxies     : Vector[P]                = j.stripBank.strips.indices.map(obj).toVector
  val sliderParams: Vector[Parameter]        = proxies.map(param)
  val paramState  : mutable.ArrayBuffer[Any] = mutable.ArrayBuffer.fill(8)(State.Normal)

  val barMode: BarMode
  val paramKnowsValue: Boolean             = true // UserControls don't and that's sad
  val paramValueCache: ArrayBuffer[Double] = mutable.ArrayBuffer.fill(8)(0.0) // unscaled

  def paramValue(idx: Int): Double =
    if (paramKnowsValue) sliderParams(idx).value().get() else paramValueCache(idx)
  def paramRange(idx: Int): (Double, Double) = (0.0, 1.0)

  def bindWithRange(idx: Int, force: Boolean = false): Unit =
    if (force || paramState(idx) == State.Normal) { // check state when binding from outside
      val (min, max) = paramRange(idx)
      j.stripBank.strips(idx).slider.setBindingWithRange(sliderParams(idx), min, max)
    }


  def stripObserver(idx: Int): DoubleValueChangedCallback =
    (v: Double) =>
      if (isOn) {
        j.stripBank.setValue(idx,
          (1.0.min(v / paramRange(idx)._2) * 127).toInt)
        paramValueCache.update(idx, v)
      }

  override def modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.indices.flatMap { idx =>
    val strip: JamTouchStrip = j.stripBank.strips(idx)
    val proxy: ObjectProxy   = proxies(idx)
    val param: Parameter     = sliderParams(idx)

    param.markInterested()
    param.name().markInterested()
    proxy.exists().markInterested()

    var offsetObserver: Double => Unit = _ => ()
    strip.slider.value().addValueObserver(offsetObserver(_))

    var startValue: Option[Double] = None

    def engage(event: Event): Unit = {
      import Event._
      import State._

      val shiftOn = j.Modifiers.Shift.isPressed()
      val stripOn = strip.slider.isBeingTouched.get()

      val state = (shiftOn, stripOn, event, paramState(idx)) match {
        case (_,_,ShiftP, Normal) =>
          strip.slider.clearBindings()
          ShiftTracking
        case (true, true, _:PressEvent, state) =>
          if (state == Normal)
            strip.slider.clearBindings()
          val current = param.get()
          startValue = None
          offsetObserver = { v: Double =>
            val offset = (v - startValue.getOrElse(v)) * 0.2
            param.set(current + offset)
            if (startValue.isEmpty) startValue = Some(v)
          }
          ShiftTracking
        case (true,_,StripR,ShiftTracking) =>
          offsetObserver = _ => ()
          ShiftTracking
        case (_,_,_:ReleaseEvent,ShiftTracking) =>
          offsetObserver = _ => ()
          bindWithRange(idx, force = true)
          Normal
        case _ =>
          Normal
      }
      paramState.update(idx, state)
    }

    proxy.exists().addValueObserver(v => if (isOn) j.stripBank.setActive(idx, v))

    // move slider
    if (paramKnowsValue) {
      param.value().markInterested()
      param.value().addValueObserver(stripObserver(idx))
    } else {
      val tv = strip.slider.targetValue()
      val hv = strip.slider.hasTargetValue
      tv.markInterested()
      hv.markInterested()
      tv.addValueObserver(v => if(hv.get()) stripObserver(idx).valueChanged(v))
      hv.addValueObserver(v => if (v) stripObserver(idx).valueChanged(tv.get()))
    }

    proxy match {
      case channel: Channel       =>
        channel.color().markInterested()
        channel.color().addValueObserver((r, g, b) =>
          if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
      case send: Send             =>
        send.sendChannelColor().markInterested()
        send.sendChannelColor().addValueObserver((r, g, b) =>
          if (isOn) j.stripBank.setColor(idx, NIColorUtil.convertColor(r, g, b)))
      case _: RemoteControl =>
      case _: Parameter => ()
    }

    import Event._
    if (paramKnowsValue)
      Vector(
        HB(j.Modifiers.Shift.pressedAction, s"shift $idx pressed", () => engage(ShiftP), tracked = false, BindingBehavior(exclusive = false)),
        HB(j.Modifiers.Shift.releasedAction, s"shift $idx released", () => engage(ShiftR), tracked = false, BindingBehavior(exclusive = false)),
        HB(strip.slider.beginTouchAction, s"shift-strip $idx pressed", () => engage(StripP), tracked = false, BindingBehavior(exclusive = false)),
        HB(strip.slider.endTouchAction, s"shift-strip $idx released", () => engage(StripR), tracked = false, BindingBehavior(exclusive = false)),
      )
    else Vector.empty
  }

  private def sync(idx: Int, flush: Boolean = true): Unit = {
    j.stripBank.setValue(idx,
      (paramValue(idx) * 127).toInt,
      flush)
  }

  override def activate(): Unit = {

    //ext.host.println(barMode.toString)
    //ext.host.println(sliderParams.map(_.name().get()).mkString(","))
    //ext.host.println(sliderParams.map(_.value().get()).mkString(","))

    paramState.mapInPlace(_ => State.Normal)
    j.stripBank.barMode = barMode

    j.stripBank.strips.forindex { case (strip, idx) =>
      val proxy = proxies(idx)

      (proxy match {
        case channel: Channel =>
          Some(JamColorState.toColorIndex(channel.color().get()))
        case send: Send       =>
          Some(JamColorState.toColorIndex(send.sendChannelColor().get()))
        case _: RemoteControl =>
          Some(rainbow(idx))
        case _: Parameter =>
          // a random parameter we know nothing about (probably from UserControlBank)
          Some(JAMColorBase.RED)
      }).foreach(c => j.stripBank.setColor(idx, c))

      j.stripBank.setActive(idx, value = proxy.exists().get, flush = false)
    }

    j.stripBank.flushColors()

    sliderParams.indices.foreach(sync(_, false))
    if (barMode == BarMode.DUAL)
      j.stripBank.flushValues()

    j.stripBank.strips.indices.foreach(bindWithRange(_))

    super.activate()
  }

  override def deactivate(): Unit = {
    super.deactivate()
    j.stripBank.strips.foreach(_.slider.clearBindings())
  }
}

object SliderBankMode {
  sealed trait Event
  sealed trait ShiftEvent extends Event
  sealed trait StripEvent extends Event
  sealed trait PressEvent extends Event
  sealed trait ReleaseEvent extends Event
  object Event {
    case object ShiftP extends ShiftEvent with PressEvent
    case object ShiftR extends ShiftEvent with ReleaseEvent
    case object StripP extends StripEvent with PressEvent
    case object StripR extends StripEvent with ReleaseEvent
  }

  sealed trait State
  object State {
    case object ShiftTracking extends State
    case object Normal extends State
  }

  sealed trait TrackedValue
  object TrackedValue {
    case object ParamValue extends TrackedValue
    case object TargetValue extends TrackedValue
  }
}
