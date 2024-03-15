package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.*
import com.github.unthingable.Util
import com.github.unthingable.Util.trace
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.HB.BindingOps
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.CycleMode
import com.github.unthingable.framework.mode.ModeButtonCycleLayer
import com.github.unthingable.framework.mode.ModeLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.*
import com.github.unthingable.jam.surface.BlackSysexMagic.BarMode
import com.github.unthingable.jam.surface.HasHwButton
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.util.FilteredPage

import java.time.Duration
import java.time.Instant
import java.util.function.BooleanSupplier

trait Control:
  this: Jam with MacroL =>
  // devices!
  trait UserControlPages:
    def selectUser(): Unit
    def isUserSelected: Boolean

  lazy val controlLayer: ModeButtonCycleLayer with UserControlPages =
    new ModeButtonCycleLayer("CONTROL", j.control, CycleMode.Select) with UserControlPages:
      val touchFX                      = "MonsterFX"
      val device: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()
      val page: FilteredPage           = FilteredPage(device.createCursorRemoteControlsPage(8), _ != touchFX)
      var currentUserPage: Int         = 0
      var previousUserPage: Int        = 0
      val userOffset: Int              = 9 // 1 normal slider bank + 8 slices
      var currentSlice: Int            = 0
      var previousSlice: Int           = 0

      device.hasNext.markInterested()
      device.hasPrevious.markInterested()
      page.c.pageNames().markInterested()
      page.c.selectedPageIndex().markInterested()

      // experimenting with remotes
      // page.c
      //   .getName()
      //   .addValueObserver(s => Util.println(s"XXX cursor remote page name now $s, ${trackPage.equals(page.c)}"))
      // device.name().addValueObserver(s => Util.println(s"XXX cursor device now $s"))
      // device.isNested().addValueObserver(b => Util.println(s"XXX cursor device enabled: $b"))
      // val trackPage: CursorRemoteControlsPage = ext.cursorTrack.createCursorRemoteControlsPage(8)
      // trackPage
      //   .getName()
      //   .addValueObserver(s => Util.println(s"XXX TRACK remote page name now $s, ${trackPage.equals(page.c)}"))
      // trackPage
      //   .pageCount()
      //   .addValueObserver(s => Util.println(s"XXX TRACK remote page COUNT now $s"))
      // trackPage
      //   .selectedPageIndex()
      //   .addValueObserver(s => Util.println(s"XXX TRACK remote page INDEX now $s"))

      val secondCursor: CursorRemoteControlsPage =
        device.createCursorRemoteControlsPage(touchFX, 8, "")
      var touchPage: Option[CursorRemoteControlsPage] = None
      secondCursor.pageNames().markInterested()
      secondCursor.selectedPageIndex().markInterested()
      secondCursor
        .pageNames()
        .addValueObserver(names =>
          touchPage = names.zipWithIndex.find(_._1 == touchFX).map {
            case (_, idx) =>
              secondCursor.selectedPageIndex().set(idx)
              secondCursor
          }
        )

      val userBank: UserControlBank = ext.host.createUserControls(64)

      /* User mode */
      override def lightOn: BooleanSupplier = () =>
        if deviceSelector.isOn && deviceSelector.selected.contains(0) then j.Mod.blink
        else isOn

      def selectUser(): Unit = select(currentUserPage + userOffset)

      def isUserSelected: Boolean = selected.exists(_ >= userOffset)

      override val subModes: Vector[ModeLayer] =
        ((
          new SliderBankMode(
            "strips remote",
            page.c.getParameter,
            JamParameter.Regular.apply,
            Seq.fill(8)(BarMode.DUAL)
          ):

            j.stripBank.strips.forindex {
              case (strip, idx) =>
                strip.slider.isBeingTouched.markInterested()
                strip.slider.isBeingTouched.addValueObserver(v =>
                  if isOn then
                    touchPage.foreach(tp =>
                      if idx < tp.getParameterCount then tp.getParameter(idx).value().set(if v then 1 else 0)
                    )
                )
                val param = sliderParams(idx)
                param.p.modulatedValue().markInterested()
                param.p
                  .modulatedValue()
                  .addValueObserver(v => if isOn then strip.update((v * 127).toInt))
            }

            override def onActivate(): Unit =
              sliderParams.forindex {
                case (param, idx) =>
                  j.stripBank.strips(idx).update((param.p.modulatedValue().get() * 127).toInt)
              }
              super.onActivate()

            override val modeBindings: Seq[Binding[?, ?, ?]] = super.modeBindings ++
              Vector(
                SupBooleanB(j.left.light, m(() => device.hasPrevious.get(), page.hasPrevious)),
                SupBooleanB(j.right.light, m(() => device.hasNext.get(), page.hasNext)),
                EB(
                  j.left.st.release,
                  "scroll left",
                  m(() => device.selectPrevious(), page.selectPrevious)
                ),
                EB(j.right.st.release, "scroll right", m(() => device.selectNext(), page.selectNext)),
                // FIXME make combo
                EB(j.left.st.press, "left", () => if j.right.st.isPressed then select(currentSlice + 1)),
                EB(j.right.st.press, "right", () => if j.left.st.isPressed then select(currentSlice + 1)),
              )
        ) +:
          EIGHT.map(idx =>
            new SliderBankMode(
              s"strips slice $idx",
              obj = i => trackBank.getItemAt(i).createCursorDevice(),
              param = p => JamParameter.Regular(p.createCursorRemoteControlsPage(8).getParameter(idx)),
              barMode = Seq.fill(8)(BarMode.DUAL),
              stripColor = Some(_ => Util.rainbow8(idx))
            ):
              j.stripBank.strips.forindex {
                case (strip, stripIdx) =>
                  strip.slider.isBeingTouched.markInterested()

                  // find touch pages
                  val cursor = obj(stripIdx).createCursorRemoteControlsPage(touchFX, 8, "")
                  var localTouchPage: Option[CursorRemoteControlsPage] = None
                  cursor.pageNames().markInterested()
                  cursor.selectedPageIndex().markInterested()

                  cursor.pageNames().addValueObserver { names =>
                    localTouchPage = names.zipWithIndex.find(_._1 == touchFX).map {
                      case (_, page) =>
                        cursor.selectedPageIndex().set(page)
                        cursor
                    }
                  }

                  strip.slider.isBeingTouched.addValueObserver(v =>
                    if isOn then
                      localTouchPage.foreach(tp =>
                        if idx < tp.getParameterCount then tp.getParameter(idx).value().set(if v then 1 else 0)
                      )
                  )

                  val param = sliderParams(stripIdx)
                  param.p.modulatedValue().markInterested()
                  param.p
                    .modulatedValue()
                    .addValueObserver(v => if isOn then strip.update((v * 127).toInt))
              }

              var (pressL, pressR) = (false, false)

              override def onActivate(): Unit =
                currentSlice = idx
                sliderParams.forindex {
                  case (param, idx) =>
                    j.stripBank.strips(idx).update((param.p.modulatedValue().get() * 127).toInt)
                }
                pressL = false
                pressR = false
                super.onActivate()

              val superBindings = super.modeBindings // cache for dirty check

              override val modeBindings: Seq[Binding[?, ?, ?]] = superBindings ++
                Vector(
                  SupBooleanB(j.left.light, () => true),
                  SupBooleanB(j.right.light, () => true),
                  // must press both and then release to deactivate, so that releases don't end up in remote layer
                  EB(j.left.st.press, "slice left press", () => pressL = true),
                  EB(j.right.st.press, "slice right press", () => pressR = true),
                  EB(
                    j.left.st.release,
                    "slice left release",
                    () => if pressL && !j.right.st.isPressed then select(0)
                  ),
                  EB(
                    j.right.st.release,
                    "slice right release",
                    () => if pressR && !j.left.st.isPressed then select(0)
                  ),
                ) ++
                EIGHT.flatMap { idx =>
                  val button = j.groupButtons(idx)
                  Vector(
                    EB(button.st.press, s"control slice $idx", () => selectSlice(idx)),
                    EB(
                      button.st.release,
                      s"control slice $idx release",
                      () =>
                        if isOlderThan(Duration.ofMillis(500)) || hasDirtyBindings(superBindings*)
                        then selectSlice(previousSlice)
                    ),
                    SupColorStateB(
                      button.light,
                      () =>
                        JamColorState(
                          if selected.contains(idx + 1) then JamColorBase.WHITE else Util.rainbow8(idx),
                          if selected.contains(idx + 1) then 3 else 0
                        )
                    )
                  )
                }
          )) ++
          EIGHT.map { idx =>
            new SliderBankMode(
              s"strips user bank $idx",
              i => userBank.getControl(i + idx),
              JamParameter.UserControl.apply,
              barMode = Seq.fill(8)(BarMode.SINGLE),
            ):

              val superBindings = super.modeBindings // cache for dirty check
              // .trace(bb => s"user bank $idx bindings: ${bb.outBindings.map(_.name).mkString(", ")}")

              override val modeBindings: Seq[Binding[?, ?, ?]] = superBindings ++
                Vector(
                  SupBooleanB(j.macroButton.light, () => true)
                ) ++
                EIGHT.flatMap(idx =>
                  Vector(
                    EB(
                      j.groupButtons(idx).st.press,
                      s"user bank $idx",
                      () =>
                        previousUserPage = currentUserPage
                        currentUserPage = idx
                        selectUser()
                    ),
                    EB(
                      j.groupButtons(idx).st.release,
                      s"user bank $idx release",
                      () =>
                        if isOlderThan(Duration.ofMillis(500)) || hasDirtyBindings(superBindings*)
                        then
                          currentUserPage = previousUserPage
                          selectUser()
                    ),
                    SupColorStateB(
                      j.groupButtons(idx).light,
                      () =>
                        if currentUserPage == idx then JamColorState(JamColorBase.WHITE, 3)
                        else JamColorState(JamColorBase.WHITE, 0)
                    )
                  )
                )
          }

      /* Control mode */
      def m(default: () => Boolean, modePressed: () => Boolean): BooleanSupplier =
        () => if modeButton.asInstanceOf[HasHwButton].btn.isPressed().get then modePressed() else default()

      def m(default: () => Unit, modePressed: () => Unit): () => Unit =
        () => if modeButton.asInstanceOf[HasHwButton].btn.isPressed().get then modePressed() else default()

      def selectSlice(slice: Int): Unit =
        previousSlice = currentSlice
        select(slice + 1)

      /* Main */
      override def onActivate(): Unit =
        val idx       = page.c.selectedPageIndex().get()
        val pageNames = page.c.pageNames().get()
        if idx >= 0 && idx < pageNames.length && pageNames(idx) == touchFX then
          if page.hasPrevious() then page.selectPrevious()
          else page.selectNext()
        super.onActivate()

      override def onDeactivate(): Unit =
        ext.events.eval("control onDeactivate")(
          deviceSelector.deactivateEvent*
        ) // if it was active we don't want it
        super.onDeactivate()

  lazy val deviceSelector: ModeButtonCycleLayer = new ModeButtonCycleLayer(
    "deviceSelector",
    j.control,
    CycleMode.Sticky,
    silent = true,
    siblingOperatedModes = controlLayer.subModes
  ):
    val cursorDevice: PinnableCursorDevice = ext.cursorTrack.createCursorDevice()

    override def operatedBindings: Iterable[Binding[?, ?, ?]] =
      super.operatedBindings ++ macroLayer.loadBindings

    override val subModes: Vector[ModeLayer] = Vector(
      // all device matrix
      new SimpleModeLayer("device matrixSelector"):
        val deviceBanks: Seq[DeviceBank]             = EIGHT.map(trackBank.getItemAt).map(_.createDeviceBank(8))
        var sourceDevice: Option[Util.Timed[Device]] = None

        deviceBanks.foreach { bank =>
          bank.canScrollForwards.markInterested()
          bank.canScrollBackwards.markInterested()
        }

        def selectDevice(trackIdx: Int, device: Device): Unit =
          Util.println(
            s"** select/clear/dup " +
              Seq(GlobalMode.Select, GlobalMode.Clear, GlobalMode.Duplicate)
                .map(_.isOn)
                .mkString("/")
          )
          device.exists().get() match
            case true if GlobalMode.Select.isOn => device.isEnabled.toggle()
            case true if GlobalMode.Clear.isOn  => device.deleteObject()
            case exists if GlobalMode.Duplicate.isOn =>
              sourceDevice match
                case Some(Util.Timed(source, instant)) if GlobalMode.Duplicate.isOlderThan(instant) =>
                  if exists then device.beforeDeviceInsertionPoint().copyDevices(source)
                  else
                    trackBank
                      .getItemAt(trackIdx)
                      .endOfDeviceChainInsertionPoint()
                      .copyDevices(source)
                  sourceDevice = None
                case _ => sourceDevice = Some(Util.Timed(device, Instant.now()))
            case _ =>
              ext.cursorTrack.selectChannel(trackBank.getItemAt(trackIdx))
              // cursorDevice.selectDevice(device)
              device.selectInEditor()
            // deviceBanks(trackIdx).scrollIntoView(device.position().get())
        end selectDevice

        def deviceColor(device: Device): Int =
          (device.isPlugin.get(), device.deviceType().get()) match
            case (false, "audio-effect") => JamColorBase.ORANGE
            case (false, "instrument")   => JamColorBase.WARM_YELLOW
            case (false, "note-effect")  => JamColorBase.CYAN
            case (true, "audio-effect")  => JamColorBase.MAGENTA
            case (true, "instrument")    => JamColorBase.LIME
            case (true, "note-effect")   => JamColorBase.PLUM
            case (_, s) =>
              Util.println(s"unknown device $s")
              JamColorBase.RED

        override val modeBindings: Seq[Binding[?, ?, ?]] = deviceBanks.zipWithIndex.flatMap {
          case (bank, col) =>
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
              // def isSelected = isSelectedEV.get() &&

              Vector(
                EB(mButton.st.press, s"select device $col:$row", () => selectDevice(col, device)),
                EB(mButton.st.release, s"noop $col:$row", () => ()),
                SupColorStateB(
                  mButton.light,
                  () =>
                    if device.exists().get() then
                      JamColorState(
                        deviceColor(device),
                        (isSelected.get(), device.isEnabled.get()) match
                          case (true, true)   => if j.Mod.blink3 then 3 else 0
                          case (true, false)  => if j.Mod.blink3 then 2 else 0
                          case (false, true)  => 2
                          case (false, false) => 0
                      )
                    else JamColorState.empty,
                  JamColorState.empty
                ),
              )
            }
        } ++
          Vector(
            SupBooleanB(j.dpad.up.light, () => deviceBanks.exists(_.canScrollBackwards.get())),
            SupBooleanB(j.dpad.down.light, () => deviceBanks.exists(_.canScrollForwards.get())),
            EB(
              j.dpad.up.st.press,
              "device bank up",
              () => deviceBanks.foreach(_.scrollPageBackwards())
            ),
            EB(
              j.dpad.down.st.press,
              "device bank down",
              () => deviceBanks.foreach(_.scrollPageForwards())
            ),
          )
      ,
      // noop mode (disable device selector)
      new SimpleModeLayer("device noopSelector"):
        override val modeBindings: Seq[Binding[?, ?, ?]] = Vector.empty
      ,
    )
    override val modeBindings: Seq[Binding[?, ?, ?]] = super.modeBindings ++
      Vector(
        EB(
          j.select.st.press,
          "cycle device selectors",
          () => if j.control.st.isPressed then cycle(),
          BB.omni
        ),
        // HB(j.macroButton.pressedAction, "control userbank cycle", () => deviceLayer.cycle()),
      )
end Control
