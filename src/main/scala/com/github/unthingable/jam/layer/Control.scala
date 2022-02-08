package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.{CursorDevice, CursorRemoteControlsPage, Device, DeviceBank, Parameter, PinnableCursorDevice, RemoteControl, Track, UserControlBank}
import com.github.unthingable.{FilteredPage, Util}
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.JamColor.JAMColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.{Binding, BindingBehavior, CycleMode, HB, IntActivatedLayer, Jam, ModeCycleLayer, ModeLayer, SimpleModeLayer, SliderBankMode, SubModeLayer, SupBooleanB, SupColorStateB}

import java.time.{Duration, Instant}
import java.util.function.BooleanSupplier

trait Control { this: Jam with MacroL =>
  // devices!
  trait UserControlPages {
    def selectUser(): Unit
    def isUserSelected: Boolean
  }

  lazy val controlLayer: ModeCycleLayer with UserControlPages = new ModeCycleLayer("CONTROL", j.control, CycleMode.Select) with UserControlPages {
    val touchFX                      = "MonsterFX"
    val device: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()
    val page  : FilteredPage         = FilteredPage(
      device.createCursorRemoteControlsPage(8),
      _ != touchFX)
    var currentUserPage: Int         = 0
    val userOffset: Int = 9 // 1 normal slider bank + 8 slices
    var currentSlice: Int = 0
    var previousSlice: Int = 0

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

    val userBank: UserControlBank = ext.host.createUserControls(64)

    /* User mode */
    override def lightOn: BooleanSupplier = () =>
      if (deviceSelector.isOn && deviceSelector.selected.contains(0))
        j.Modifiers.blink
      else
        isOn

    def selectUser(): Unit = select(currentUserPage + userOffset)

    def isUserSelected: Boolean = selected.exists(_ >= userOffset)

    override val subModes: Seq[SubModeLayer] = (
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
          HB(j.left.releasedAction, "scroll left", m(() => device.selectPrevious(), page.selectPrevious)),
          HB(j.right.releasedAction, "scroll right", m(() => device.selectNext(), page.selectNext)),
          HB(j.left.pressedAction, "left", () => if (j.right.isPressed()) select(currentSlice + 1)),
          HB(j.right.pressedAction, "right", () => if (j.left.isPressed()) select(currentSlice + 1)),
        )
      } +:
        EIGHT.map(idx => new SliderBankMode[CursorDevice](
          s"strips slice $idx",
          obj = i => trackBank.getItemAt(i).createCursorDevice(),
          param = _.createCursorRemoteControlsPage(8).getParameter(idx),
          stripColor = Some(_ => Util.rainbow(idx))
        ) {
          override val barMode: BarMode = BarMode.DUAL

          j.stripBank.strips.forindex { case (strip, stripIdx) =>
            strip.slider.isBeingTouched.markInterested()

            // find touch pages
            val cursor = obj(stripIdx).createCursorRemoteControlsPage(touchFX, 8, "")
            var localTouchPage: Option[CursorRemoteControlsPage] = None
            cursor.pageNames().markInterested()
            cursor.selectedPageIndex().markInterested()

            cursor.pageNames().addValueObserver { names =>
              localTouchPage = names
                .zipWithIndex.find(_._1 == touchFX).map { case (_, page) =>
                cursor.selectedPageIndex().set(page)
                cursor
              }
            }

            strip.slider.isBeingTouched.addValueObserver(v => if (isOn) localTouchPage.foreach(tp =>
              if (idx < tp.getParameterCount) tp.getParameter(idx).value().set(if (v) 1 else 0)))

            val param = sliderParams(stripIdx)
            param.modulatedValue().markInterested()
            param.modulatedValue().addValueObserver(v => if (isOn) strip.update((v * 127).toInt))
          }

          var (pressL, pressR) = (false, false)

          override def activate(): Unit = {
            currentSlice = idx
            sliderParams.forindex { case (param, idx) =>
              j.stripBank.strips(idx).update((param.modulatedValue().get() * 127).toInt)
            }
            pressL = false
            pressR = false
            super.activate()
          }

          override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
            SupBooleanB(j.left.light.isOn, () => true),
            SupBooleanB(j.right.light.isOn, () => true),
            // must press both and then release to deactivate, so that releases don't end up in remote layer
            HB(j.left.pressedAction, "slice left press", () => pressL = true),
            HB(j.right.pressedAction, "slice right press", () => pressR = true),
            HB(j.left.releasedAction, "slice left release", () => if (pressL && !j.right.isPressed()) select(0)),
            HB(j.right.releasedAction, "slice right release", () => if (pressR && !j.left.isPressed()) select(0)),
          ) ++ EIGHT.flatMap { idx =>
            val button = j.groupButtons(idx)
            Vector(
              HB(button.pressedAction, s"control slice $idx", () => selectSlice(idx)),
              HB(button.releasedAction, s"control slice $idx release", () =>
                if (Instant.now().isAfter(activeAt.plus(Duration.ofMillis(500))) || modeBindings.outBindings.exists(_.operatedAt.exists(_.isAfter(activeAt))))
                  selectSlice(previousSlice)
              ),
              SupColorStateB(button.light, () => JamColorState(
                if (selected.contains(idx + 1)) JAMColorBase.WHITE else Util.rainbow(idx),
                if (selected.contains(idx + 1)) 3 else 0
              ))
          )}
        })
      ) ++ EIGHT.map { idx =>
        new SliderBankMode[Parameter](s"strips user bank $idx", i => userBank.getControl(i + idx), identity) {
          override val barMode        : BarMode = BarMode.SINGLE
          override val paramKnowsValue: Boolean = false
          var previousUserPage: Int = 0

          override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
            SupBooleanB(j.macroButton.light.isOn, () => true)
          ) ++ EIGHT.flatMap(idx => Vector(
            HB(j.groupButtons(idx).pressedAction, s"user bank $idx", () => {
              previousUserPage = currentUserPage
              currentUserPage = idx
              selectUser()
            }),
            HB(j.groupButtons(idx).releasedAction, s"user bank $idx release", () =>
              if (Instant.now().isAfter(activeAt.plus(Duration.ofMillis(500))) || modeBindings.outBindings.exists(_.operatedAt.exists(_.isAfter(activeAt)))) {
                currentUserPage = previousUserPage
                selectUser()
              }
            ),
            SupColorStateB(j.groupButtons(idx).light, () =>
              if (currentUserPage == idx)
                JamColorState(JAMColorBase.WHITE, 3)
              else
                JamColorState(JAMColorBase.WHITE, 0))
          ))
        }
      }

    /* Control mode */
    def m(default: () => Boolean, modePressed: () => Boolean): BooleanSupplier =
      () => if (modeButton.isPressed()) modePressed() else default()

    def m(default: () => Unit, modePressed: () => Unit): () => Unit =
      () => if (modeButton.isPressed()) modePressed() else default()

    def selectSlice(slice: Int): Unit = {
      previousSlice = currentSlice
      select(slice + 1)
    }

    /* Main */
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

  lazy val deviceSelector: ModeCycleLayer = new ModeCycleLayer(
    "deviceSelector",
    j.control,
    CycleMode.Sticky,
    silent = true,
    siblingOperatedModes = controlLayer.subModes
  ) {
    val cursorDevice: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()

    override def operatedBindings: Iterable[Binding[_, _, _]] = super.operatedBindings ++ macroLayer.loadBindings

    override val subModes    : Seq[ModeLayer with IntActivatedLayer] = Vector(
      // all device matrix
      new SimpleModeLayer("device matrixSelector") with IntActivatedLayer {
        val deviceBanks: Seq[DeviceBank] = EIGHT.map(trackBank.getItemAt).map(_.createDeviceBank(8))
        var sourceDevice: Option[Util.Timed[Device]] = None

        deviceBanks.foreach { bank =>
          bank.canScrollForwards.markInterested()
          bank.canScrollBackwards.markInterested()
        }

        def selectDevice(trackIdx: Int, device: Device): Unit = {
          Util.println(s"** select/clear/dup " + Seq(GlobalMode.Select, GlobalMode.Clear, GlobalMode.Duplicate).map(_.isOn).mkString("/"))
          device.exists().get() match {
            case true if GlobalMode.Select.isOn => device.isEnabled.toggle()
            case true if GlobalMode.Clear.isOn  => device.deleteObject()
            case exists if GlobalMode.Duplicate.isOn =>
              sourceDevice match {
                case Some(Util.Timed(source, instant)) if instant.isAfter(GlobalMode.Duplicate.activeAt) =>
                  if (exists) device.beforeDeviceInsertionPoint().copyDevices(source)
                  else trackBank.getItemAt(trackIdx).endOfDeviceChainInsertionPoint().copyDevices(source)
                  sourceDevice = None
                case _ => sourceDevice = Some(Util.Timed(device, Instant.now()))
              }
            case _ =>
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
            Util.println(s"unknown device $s")
            JAMColorBase.RED
        }

        override val modeBindings: Seq[Binding[_, _, _]] = deviceBanks.zipWithIndex.flatMap { case (bank, col) =>
          EIGHT.flatMap { row =>
            val mButton = j.matrix(row)(col)
            val device  = bank.getItemAt(row)
            device.exists().markInterested()
            device.isEnabled.markInterested()
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
                        JamColorState(
                          deviceColor(device),
                          (isSelected.get(), device.isEnabled.get()) match {
                            case (true, true) => if (j.Modifiers.blink3) 3 else 0
                            case (true, false) => if (j.Modifiers.blink3) 2 else 0
                            case (false, true) => 2
                            case (false, false) => 0
                          })
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
      // noop mode (disable device selector)
      new SimpleModeLayer("device noopSelector") with IntActivatedLayer {
        override val modeBindings: Seq[Binding[_, _, _]] = Vector.empty
      },

    )
    override val modeBindings: Seq[Binding[_, _, _]] = super.modeBindings ++ Vector(
      HB(j.select.pressedAction, "cycle device selectors", () => if (j.control.isPressed()) cycle(),
        tracked = false,
        behavior = BindingBehavior(exclusive = false)),
      //HB(j.macroButton.pressedAction, "control userbank cycle", () => deviceLayer.cycle()),
    )
  }

}
