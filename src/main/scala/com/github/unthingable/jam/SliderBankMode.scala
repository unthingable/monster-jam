package com.github.unthingable.jam

import com.bitwig.extension.callback.DoubleValueChangedCallback
import com.bitwig.extension.controller.api.Channel
import com.bitwig.extension.controller.api.Device
import com.bitwig.extension.controller.api.HardwareSlider
import com.bitwig.extension.controller.api.ObjectProxy
import com.bitwig.extension.controller.api.Parameter
import com.bitwig.extension.controller.api.RemoteControl
import com.bitwig.extension.controller.api.Send
import com.github.unthingable.MonsterJamExt
import com.github.unthingable.Util
import com.github.unthingable.Util.safeMap
import com.github.unthingable.framework.RefSubSelective
import com.github.unthingable.framework.binding.Bindable
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.JamParameter.*
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamSurface
import com.github.unthingable.jam.surface.JamTouchStrip
import com.github.unthingable.jam.surface.NIColorUtil

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import SliderBankMode.*

case class PRange(min: Double, max: Double):
  val size = max - min

sealed trait JamParameter derives CanEqual

object JamParameter:
  sealed trait WithParam extends JamParameter:
    val p: Parameter

  case class Internal(callback: Double => Unit) extends JamParameter
  case class Regular(p: Parameter)              extends WithParam
  case class UserControl(p: Parameter)          extends WithParam
  case object Empty                             extends JamParameter

/** A single wrapper around slider, internal value and (optional) host parameter, with operations for sync and update */
sealed trait SliderOp extends RefSubSelective[SliderOp.Source, Double] with Bindable:
  def reset(): Unit

  /** Update internal value from bound parameter */
  def pull(): Unit

object SliderOp:
  import JamParameter.*

  enum Source:
    case Param, Slider, Internal, Init

  def apply(
    idx: Int, // for debugging
    slider: HardwareSlider,
    updateLed: Int => Unit,
    param: JamParameter,
    range: => PRange, // lower/upper value limit corresponding to 0/127
    isParentOn: => Boolean
  )(using j: JamSurface, ext: MonsterJamExt): SliderOp = param match
    case Regular(p) =>
      new SliderOpBase(idx, slider, updateLed, range, isParentOn):
        p.value.markInterested()
        p.name().markInterested()
        p.value().addValueObserver((v: Double) => if isParentOn then set(v, Source.Param))

        override def paramListeners: Seq[Listener] = Seq(
          Some(Source.Param) -> ((v: Double) => if isParentOn then p.value().set(v))
        )

        override inline def bind(): Unit =
          _range = range
          slider.setBindingWithRange(p, _range.min, _range.max)

        override inline def clear(): Unit = slider.clearBindings()

        override inline def reset(): Unit = p.reset()

        override inline def pull(): Unit = set(p.value().get(), Source.Param)

    case UserControl(p) =>
      new SliderOpBase(idx, slider, updateLed, range, isParentOn):
        private val tv = slider.targetValue()
        private val hv = slider.hasTargetValue()
        tv.markInterested()
        hv.markInterested()
        tv.addValueObserver(v => if isParentOn && hv.get() then set(v, Source.Param))
        hv.addValueObserver(v => if isParentOn then set(if v then tv.get() else get, Source.Param))

        override def paramListeners: Seq[Listener] = Seq.empty

        override inline def bind(): Unit =
          _range = range
          slider.setBindingWithRange(p, _range.min, _range.max)

        override inline def clear(): Unit =
          slider.clearBindings()

        override inline def reset(): Unit = p.reset()

        override inline def pull(): Unit = ()

    case Internal(f) =>
      new SliderOpBase(idx, slider, updateLed, range, isParentOn):
        slider.value().addValueObserver((v: Double) => if isActive then set(s2p(v), Source.Slider))

        protected var isBound: Boolean = false // slider->value connected
        protected inline def isActive  = isBound && isParentOn

        override def paramListeners: Seq[Listener] = Seq(Some(Source.Internal) -> f)

        override inline def bind(): Unit =
          _range = range
          isBound = true

        override inline def clear(): Unit = isBound = false

        override inline def reset(): Unit = ()

        override inline def pull(): Unit = push()
    case Empty =>
      new SliderOp:
        def bind(): Unit                            = ()
        def clear(): Unit                           = ()
        val init: (Double, Source)                  = (0, Source.Init)
        protected def listeners: Iterable[Listener] = Vector.empty
        def pull(): Unit                            = ()
        def reset(): Unit                           = ()
end SliderOp

// value is unscaled parameter value
abstract class SliderOpBase(
  val idx: Int, // for debugging
  slider: HardwareSlider,
  updateLed: Int => Unit,
  range: => PRange, // lower/upper value limit corresponding to 0/127
  isParentOn: => Boolean
) extends SliderOp:
  import SliderOp.*

  protected var _range: PRange = range // efficiency cache

  override def init = (0, Source.Init)

  // assemble listeners
  protected def paramListeners: Seq[Listener]

  override protected val listeners: Iterable[Listener] = paramListeners :+
    (
      None -> ((v: Double) => if isParentOn then updateLed(p2s(v)))
    )
  // :+ (
  //   None -> (v => Util.println(s"$idx $value"))
  // )

  // scale param->slider
  def p2s(v: Double): Int = (((v - _range.min) / _range.size).min(1).max(0) * 127).toInt

  def s2p(v: Double): Double =
    v * _range.size + _range.min
end SliderOpBase

class SliderBankMode[Proxy, P <: JamParameter](
  override val id: String,
  val obj: Int => Proxy,
  val param: Proxy => P,
  barMode: => Seq[BarMode],
  stripColor: Option[Int => Int] = None,
)(using
  ext: MonsterJamExt,
  j: JamSurface,
  exists: Exists[Proxy]
) extends SimpleModeLayer(id)
    with Util:

  val proxies: Vector[Proxy]                 = j.stripBank.strips.indices.map(obj).toVector
  val sliderParams: Vector[P]                = proxies.map(param)
  val paramState: mutable.ArrayBuffer[State] = mutable.ArrayBuffer.fill(8)(State.Normal)

  def paramRange(idx: Int): PRange = PRange(0.0, 1.0)

  Util.println(s"$id sliderOps")

  val sliderOps: Vector[SliderOp] = j.stripBank.strips.zipWithIndex.map { (strip, idx) =>
    SliderOp(
      idx,
      strip.slider,
      j.stripBank.setValue(idx, _),
      sliderParams(idx),
      paramRange(idx),
      isOn && j.stripBank.active(idx)
    )
  }

  proxies.foreach(_.safeMap[ObjectProxy, Unit](_.exists().markInterested()))

  def bindWithRange(idx: Int, force: Boolean = false): Unit =
    if force || paramState(idx) == State.Normal then // check state when binding from outside
      sliderOps(idx).bind()
      sliderOps(idx).pull()

  def unbind(idx: Int): Unit =
    // bindings need clearing because they interfere with shift tracking
    sliderOps(idx).clear()

  override def modeBindings: Seq[Binding[?, ?, ?]] = j.stripBank.strips.indices.flatMap { idx =>
    val strip: JamTouchStrip = j.stripBank.strips(idx)
    val proxy: Proxy         = proxies(idx)

    var offsetObserver: Double => Unit = _ => ()
    strip.slider.value().addValueObserver(offsetObserver(_))

    val sliderOp = sliderOps(idx)

    var startValue: Option[Double] = None

    def engage(event: Event): Unit =
      import Event.*
      import State.*

      val shiftOn = j.Mod.Shift.btn.isPressed
      val stripOn = strip.slider.isBeingTouched.get()

      val state = (shiftOn, stripOn, event, paramState(idx)) match
        case (_, _, ClearP, _) =>
          unbind(idx)
          Normal
        case (_, _, ClearR, _) =>
          bindWithRange(idx, force = true)
          Normal
        case (_, _, StripP, _) if j.clear.btn.isPressed().get =>
          sliderOp.reset()
          Normal
        case (_, _, ShiftP, _) =>
          unbind(idx)
          ShiftTracking
        case (true, true, _: PressEvent, state) =>
          if state == Normal then unbind(idx)
          val current = sliderOp.get
          startValue = None
          offsetObserver = v =>
            val offset  = (v - startValue.getOrElse(v)) * 0.2
            val floored = (current + offset).max(0).min(1)
            sliderOp.set(floored, null)
            if startValue.isEmpty then startValue = Some(v)
          ShiftTracking
        case (true, _, StripR, ShiftTracking) =>
          offsetObserver = _ => ()
          ShiftTracking
        case (_, _, _: ReleaseEvent, ShiftTracking) =>
          offsetObserver = _ => ()
          bindWithRange(idx, force = true)
          Normal
        case _ =>
          Normal
      paramState.update(idx, state)
    end engage

    // if it's a regular parameter, set up an existential listener
    sliderParams(idx) match
      case Regular(p) =>
        p.exists()
          .addValueObserver(v =>
            if isOn then
              j.stripBank.setActive(idx, v)
              sliderOps(idx).pull()
          )
      case _ => ()

    proxy match
      case channel: Channel =>
        channel.color().markInterested()
        channel
          .color()
          .addValueObserver((r, g, b) => if isOn then j.stripBank.setColor(idx, NIColorUtil.convertColorX(r, g, b)))
      case send: Send =>
        send.sendChannelColor().markInterested()
        send
          .sendChannelColor()
          .addValueObserver((r, g, b) => if isOn then j.stripBank.setColor(idx, NIColorUtil.convertColorX(r, g, b)))
      case _: RemoteControl =>
      case _: Parameter     => ()
      case _: Device        => ()
      case _                => ()

    import Event.*
    Vector(
      HB(
        j.clear.btn.pressedAction,
        s"clear $idx pressed",
        () => engage(ClearP),
        BB.omni
      ),
      HB(
        j.clear.btn.releasedAction,
        s"clear $idx released",
        () => engage(ClearR),
        BB.omni
      ),
      HB(
        j.Mod.Shift.btn.pressedAction,
        s"shift $idx pressed",
        () => engage(ShiftP),
        BB.omni
      ),
      HB(
        j.Mod.Shift.btn.releasedAction,
        s"shift $idx released",
        () => engage(ShiftR),
        BB.omni
      ),
      HB(
        strip.slider.beginTouchAction,
        s"strip $idx pressed",
        () => engage(StripP),
        BB.omni
      ),
      HB(
        strip.slider.endTouchAction,
        s"strip $idx released",
        () => engage(StripR),
        BB.omni
      ),
    )
  }

  private def sync(idx: Int, flush: Boolean = true): Unit =
    sliderOps(idx).pull()

  def flushColors(): Unit =
    j.stripBank.barMode = barMode

    j.stripBank.strips.forindex {
      case (strip, idx) =>
        val proxy = proxies(idx)

        stripColor
          .map(_(idx))
          .orElse(proxy match
            case channel: Channel =>
              Some(JamColorState.toColorIndex(channel.color().get()))
            case send: Send =>
              Some(JamColorState.toColorIndex(send.sendChannelColor().get()))
            case _: RemoteControl =>
              Some(Util.rainbow8(idx))
            case _ =>
              // a random parameter we know nothing about (probably from UserControlBank)
              Some(JamColorBase.RED)
          )
          .foreach(c => j.stripBank.setColor(idx, c))

        j.stripBank.setActive(
          idx,
          value = sliderParams(idx) match // UserControls are too special, can't infer existence from proxy alone
            case UserControl(p) => true
            case _              => exists(proxy)
          ,
          flush = false
        )
    }

    j.stripBank.flushColors()
  end flushColors

  override def onActivate(): Unit =
    flushColors()
    super.onActivate()

    sliderOps.foreach(_.pull())
    j.stripBank.strips.indices.foreach(bindWithRange(_))

    // sliderParams.indices.foreach(sync(_, false))
    // if barMode.contains(BarMode.DUAL) then j.stripBank.flushValues()
    j.stripBank.flushValues()
  end onActivate

  // keeping it here while tuning slider bank transitions
  // override def onActivate(): Unit =
  //   super.onActivate()

  //   j.stripBank.strips.indices.foreach(bindWithRange(_))

  //   // sliderParams.indices.foreach(sync(_, false))
  //   // if barMode.contains(BarMode.DUAL) then j.stripBank.flushValues()
  //   (0 until 8).map(j.stripBank.setValue(_, 0, false))
  //   flushColors()
  //   sliderOps.foreach(_.pull())
  //   j.stripBank.flushValues()
  // end onActivate

  override def onDeactivate(): Unit =
    j.stripBank.setActive(_ => false)
    (0 until 8).map(j.stripBank.setValue(_, 0, false))
    j.stripBank.flushValues()
    super.onDeactivate()
    j.stripBank.strips.foreach(_.slider.clearBindings())
end SliderBankMode

object SliderBankMode:
  sealed trait Event derives CanEqual
  sealed trait ShiftEvent   extends Event
  sealed trait StripEvent   extends Event
  sealed trait PressEvent   extends Event
  sealed trait ReleaseEvent extends Event
  object Event:
    case object ShiftP extends ShiftEvent with PressEvent
    case object ShiftR extends ShiftEvent with ReleaseEvent
    case object StripP extends StripEvent with PressEvent
    case object StripR extends StripEvent with ReleaseEvent
    case object ClearP extends StripEvent with PressEvent
    case object ClearR extends StripEvent with ReleaseEvent

  enum State derives CanEqual:
    case ShiftTracking, Normal

  trait Exists[-A]:
    def apply(a: A): Boolean

  given Exists[ObjectProxy] with
    def apply(p: ObjectProxy) = p.exists().get()

  // Optional proxies are useful for skipping sliders
  given Exists[Option[?]] with
    def apply(p: Option[?]) = p.isDefined
end SliderBankMode
