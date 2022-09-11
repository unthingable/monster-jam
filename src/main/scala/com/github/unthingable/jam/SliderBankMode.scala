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
import com.github.unthingable.Util.{safeCast, safeMap}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.{JamColorState, JamSurface, JamTouchStrip, NIColorUtil}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import SliderBankMode.*
import com.github.unthingable.framework.Watched
import com.github.unthingable.framework.Ref

class SliderBankMode[Proxy, Param](
  override val id: String,
  val obj: Int => Proxy,
  val param: Proxy => Param,
  val barMode: Seq[BarMode],
  val stripColor: Option[Int => Int] = None,
)(using ext: MonsterJamExt, j: JamSurface, pValue: ParamValue[Param], exists: Exists[Proxy])
    extends SimpleModeLayer(id)
    with Util {

  val proxies: Vector[Proxy]                 = j.stripBank.strips.indices.map(obj).toVector
  val sliderParams: Vector[Param]            = proxies.map(param)
  val paramState: mutable.ArrayBuffer[State] = mutable.ArrayBuffer.fill(8)(State.Normal)

  proxies
    .zip(sliderParams)
    .foreach((proxy, param) =>
      param.safeMap[Parameter, Unit] { p =>
        // p.markInterested()
        p.value.markInterested()
      }
      proxy.safeMap[ObjectProxy, Unit](_.exists().markInterested())
    )

  val paramKnowsValue: Boolean = true // UserControls don't and that's sad
  val paramValueCache: Seq[Watched[Double]] = Seq.fill(8)(Ref(0.0)) // unscaled

  def paramValueOrCache(idx: Int): Double =
    if (paramKnowsValue) pValue.get(sliderParams(idx)) else paramValueCache(idx).get

  def paramRange(idx: Int): (Double, Double) = (0.0, 1.0)

  def bindWithRange(idx: Int, force: Boolean = false): Unit =
    if (force || paramState(idx) == State.Normal) { // check state when binding from outside
      val (min, max) = paramRange(idx)
      // ext.host.println(s"$name binding $idx with $max")

      sliderParams(idx).safeMap(j.stripBank.strips(idx).slider.setBindingWithRange(_, min, max))

      // force update to account for value lag when scrolling bank
      updateStrip(idx).valueChanged(pValue.get(sliderParams(idx)))
    }

  def unbind(idx: Int): Unit =
    // bindings need clearing because they interfere with shift tracking
    j.stripBank.strips(idx).slider.clearBindings()
    // updateStrip(idx).valueChanged(paramValueOrCache(idx))
    updateStrip(idx).valueChanged(0.5)
    ()

  def updateStrip(idx: Int): DoubleValueChangedCallback =
    (v: Double) =>
      if (isOn && !j.clear.btn.isPressed().get) {
        j.stripBank.setValue(idx, (1.0.min(v / paramRange(idx)._2) * 127).toInt)
        paramValueCache(idx).set(v)
      }

  override def modeBindings: Seq[Binding[_, _, _]] = j.stripBank.strips.indices.flatMap { idx =>
    val strip: JamTouchStrip = j.stripBank.strips(idx)
    val proxy: Proxy         = proxies(idx)
    val param: Param         = sliderParams(idx)

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
          unbind(idx)
          Normal
        case (_, _, ClearR, _) =>
          bindWithRange(idx, force = true)
          Normal
        case (_, _, StripP, _) if j.clear.btn.isPressed().get =>
          param.safeMap[Parameter, Unit](_.reset())
          Normal
        case (_, _, ShiftP, _) =>
          unbind(idx)
          ShiftTracking
        case (true, true, _: PressEvent, state) =>
          if (state == Normal)
            unbind(idx)
          val current = pValue.get(param)
          startValue = None
          offsetObserver = { v =>
            val offset  = (v - startValue.getOrElse(v)) * 0.2
            val floored = (current + offset).max(0).min(1)
            pValue.set(param, floored)
            if (startValue.isEmpty) startValue = Some(v)
          }
          ShiftTracking
        case (true, _, StripR, ShiftTracking) =>
          offsetObserver = _ => ()
          updateStrip(idx).valueChanged(pValue.get(param))
          ShiftTracking
        case (_, _, _: ReleaseEvent, ShiftTracking) =>
          offsetObserver = _ => ()
          bindWithRange(idx, force = true)
          updateStrip(idx).valueChanged(pValue.get(param))
          Normal
        case _ =>
          Normal
      }
      paramState.update(idx, state)
    }

    proxy.safeMap[ObjectProxy, Unit](
      _.exists().addValueObserver(v => if (isOn) j.stripBank.setActive(idx, v))
    )
    // proxy.exists().addValueObserver(v => if (isOn) j.stripBank.setActive(idx, v))

    // move slider dot
    if (paramKnowsValue) {
      // param.value().markInterested()
      pValue.addValueObserver(param, updateStrip(idx))
    } else {
      val tv = strip.slider.targetValue()
      val hv = strip.slider.hasTargetValue
      tv.markInterested()
      hv.markInterested()
      tv.addValueObserver(v => if (hv.get()) updateStrip(idx).valueChanged(v))
      hv.addValueObserver(v => updateStrip(idx).valueChanged(if (v) tv.get() else 0))
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
      case _                => ()
    }

    import Event._
    if (paramKnowsValue || true)
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
    else
      Vector( // for dirty tracking
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
    j.stripBank.setValue(idx, (paramValueOrCache(idx) * 127 / paramRange(idx)._2).toInt, flush)

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

        // FIXME
        j.stripBank.setActive(idx, value = exists(proxy), flush = false)
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

  enum State:
    case ShiftTracking, Normal

  trait ParamValue[-A]:
    def get(a: A): Double
    def set(a: A, v: Double): Unit
    def addValueObserver(a: A, f: DoubleValueChangedCallback): Unit

  trait Exists[-A]:
    def apply(a: A): Boolean

  given ParamValue[Parameter] with
    def get(p: Parameter): Double    = p.value().get()
    def set(p: Parameter, v: Double) = p.value().set(v)
    def addValueObserver(p: Parameter, f: DoubleValueChangedCallback) =
      p.value().addValueObserver(f)

  given Exists[ObjectProxy] with
    def apply(p: ObjectProxy) = p.exists().get()
}
