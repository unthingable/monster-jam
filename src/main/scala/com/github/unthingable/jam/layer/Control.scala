package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{CursorRemoteControlsPage, Device, DeviceBank, Parameter, PinnableCursorDevice, RemoteControl, UserControlBank}
import com.github.unthingable.FilteredPage
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.{Binding, CycleMode, HB, IntActivatedLayer, Jam, ModeCycleLayer, ModeLayer, SimpleModeLayer, SliderBankMode, SubModeLayer, SupBooleanB, SupColorStateB}

import java.util.function.BooleanSupplier

trait Control { this: Jam =>
  // devices!
  lazy val controlLayer = new ModeCycleLayer("CONTROL", j.control, CycleMode.Select) {
    val touchFX                      = "MonsterFX"
    val device: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()
    val page  : FilteredPage         = FilteredPage(
      device.createCursorRemoteControlsPage(8),
      _ != touchFX)

    device.hasNext.markInterested()
    device.hasPrevious.markInterested()
    page.c.pageNames().markInterested()
    page.c.selectedPageIndex().markInterested()

    val secondCursor: CursorRemoteControlsPage         = device.createCursorRemoteControlsPage(touchFX, 8, "")
    var touchPage   : Option[CursorRemoteControlsPage] = None
    secondCursor.pageNames().markInterested()
    secondCursor.selectedPageIndex().markInterested()
    secondCursor.pageNames().addValueObserver(names => touchPage = names
      .zipWithIndex.find(_._1 == touchFX).map { case (_, idx) =>
      secondCursor.selectedPageIndex().set(idx)
      secondCursor
    })

    val userBank: UserControlBank = ext.host.createUserControls(8)

    override val subModes: Seq[SubModeLayer] = Vector(
      new SliderBankMode[RemoteControl]("strips remote", page.c.getParameter, identity) {
        override val barMode: BarMode = BarMode.DUAL

        j.stripBank.strips.forindex { case (strip, idx) =>
          strip.slider.isBeingTouched.markInterested()
          strip.slider.isBeingTouched.addValueObserver(v => if (isOn) touchPage.foreach(tp =>
            if (idx < tp.getParameterCount) tp.getParameter(idx).value().set(if (v) 1 else 0)))
          val param = sliderParams(idx)
          param.modulatedValue().markInterested()
          param.modulatedValue().addValueObserver(v => if (isOn) strip.update((v * 127).toInt))
        }

        override def activate(): Unit = {
          sliderParams.forindex { case (param, idx) =>
            j.stripBank.strips(idx).update((param.modulatedValue().get() * 127).toInt)
          }
          super.activate()
        }

        override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
          SupBooleanB(j.left.light.isOn, m(() => device.hasPrevious.get(), page.hasPrevious)),
          SupBooleanB(j.right.light.isOn, m(() => device.hasNext.get(), page.hasNext)),
          HB(j.left.pressedAction, "scroll left", m(() => device.selectPrevious(), page.selectPrevious)),
          HB(j.right.pressedAction, "scroll right", m(() => device.selectNext(), page.selectNext)),
        )
      },
      new SliderBankMode[Parameter]("strips user bank", userBank.getControl, identity) {
        override val barMode        : BarMode = BarMode.SINGLE
        override val paramKnowsValue: Boolean = false

        override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
          SupBooleanB(j.macroButton.light.isOn, () => true)
        )
      },
    )

    def m(default: () => Boolean, modePressed: () => Boolean): BooleanSupplier =
      () => if (modeButton.isPressed()) modePressed() else default()

    def m(default: () => Unit, modePressed: () => Unit): () => Unit =
      () => if (modeButton.isPressed()) modePressed() else default()

    override def activate(): Unit = {
      val idx       = page.c.selectedPageIndex().get()
      val pageNames = page.c.pageNames().get()
      if (idx >= 0 && idx < pageNames.length && pageNames(idx) == touchFX)
        if (page.hasPrevious())
          page.selectPrevious()
        else
          page.selectNext()
      super.activate()
    }

    override def deactivate(): Unit = {
      deviceSelector.deactivateAction.invoke() // if it was active we don't want it
      super.deactivate()
    }
  }

  lazy val deviceSelector = new ModeCycleLayer("deviceSelector", j.control, CycleMode.Sticky, silent = true) {
    val cursorDevice: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()

    override val subModes    : Seq[ModeLayer with IntActivatedLayer] = Vector(
      // all device matrix
      new SimpleModeLayer("matrixSelectorSub") with IntActivatedLayer {
        val deviceBanks: Seq[DeviceBank] = EIGHT.map(trackBank.getItemAt).map(_.createDeviceBank(8))
        deviceBanks.foreach { bank =>
          bank.canScrollForwards.markInterested()
          bank.canScrollBackwards.markInterested()
        }

        def selectDevice(trackIdx: Int, device: Device): Unit = {
          if (device.exists().get()) {
            ext.cursorTrack.selectChannel(trackBank.getItemAt(trackIdx))
            //cursorDevice.selectDevice(device)
            device.selectInEditor()
            //deviceBanks(trackIdx).scrollIntoView(device.position().get())
          }
        }

        def deviceColor(device: Device): Int = (device.isPlugin.get(), device.deviceType().get()) match {
          case (false, "audio-effect") => JAMColorBase.ORANGE
          case (false, "instrument")   => JAMColorBase.WARM_YELLOW
          case (false, "note-effect")  => JAMColorBase.CYAN
          case (true, "audio-effect")  => JAMColorBase.MAGENTA
          case (true, "instrument")    => JAMColorBase.LIME
          case (true, "note-effect")   => JAMColorBase.PLUM
          case (_, s)                  =>
            ext.host.println(s"unknown device $s")
            JAMColorBase.RED
        }

        override val modeBindings: Seq[Binding[_, _, _]] = deviceBanks.zipWithIndex.flatMap { case (bank, col) =>
          EIGHT.flatMap { row =>
            val mButton = j.matrix(row)(col)
            val device  = bank.getItemAt(row)
            device.exists().markInterested()
            device.position().markInterested()
            device.deviceType().markInterested()
            device.isPlugin.markInterested()
            val isSelected = device.createEqualsValue(cursorDevice)
            isSelected.markInterested()

            Vector(
              HB(mButton.pressedAction, s"select device $col:$row", () => selectDevice(col, device)),
              HB(mButton.releasedAction, s"noop $col:$row", () => ()),
              SupColorStateB(mButton.light,
                () => if (device.exists().get())
                        JamColorState(deviceColor(device), if (isSelected.get()) 3 else 0)
                      else JamColorState.empty,
                JamColorState.empty),
            )
          }
        } ++ Vector(
          SupBooleanB(j.dpad.up.light.isOn, () => deviceBanks.exists(_.canScrollBackwards.get())),
          SupBooleanB(j.dpad.down.light.isOn, () => deviceBanks.exists(_.canScrollForwards.get())),
          HB(j.dpad.up.pressedAction, "device bank up", () => deviceBanks.foreach(_.scrollPageBackwards())),
          HB(j.dpad.down.pressedAction, "device bank down", () => deviceBanks.foreach(_.scrollPageForwards())),
        )
      },
      // device navigator - maybe todo,
      // noop mode (disable device selector)
      new SimpleModeLayer("noopSelector") with IntActivatedLayer {
        override val modeBindings: Seq[Binding[_, _, _]] = Vector.empty
      },

    )
    override val modeBindings: Seq[Binding[_, _, _]]                 = super.modeBindings ++ Vector(
      HB(j.select.pressedAction, "cycle device selectors", () => cycle()),
      //HB(j.macroButton.pressedAction, "control userbank cycle", () => deviceLayer.cycle()),
    )
  }

}
