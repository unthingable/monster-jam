package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.{Channel, HardwareActionBindable, SettableBooleanValue}
import com.github.unthingable.jam.binding.HB.action
import com.github.unthingable.jam.binding._
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.{Button, JamColorState, JamOnOffButton, JamRgbButton, OnOffButton}
import com.github.unthingable.jam.{GateMode, Jam, ModeButtonLayer, SimpleModeLayer}

trait TransportL { this: Jam =>
  lazy val position = SimpleModeLayer("position",
    Vector(HB(j.encoder.turn, "position turn", ext.host.createRelativeHardwareControlStepTarget(
      ext.transport.fastForwardAction(),
      ext.transport.rewindAction()))))

  lazy val tempoLayer = ModeButtonLayer("tempo",
    j.tempo,
    Vector(
      HB(j.encoder.turn, "tempo turn", ext.host.createRelativeHardwareControlStepTarget(
        action("inc tempo", () => ext.transport.increaseTempo(1,
          647 * (if (j.Mod.Shift.btn.isPressed()) 20 else 1))),
        action("dec tempo", () => ext.transport.increaseTempo(-1,
          647 * (if (j.Mod.Shift.btn.isPressed()) 20 else 1)))))))

  lazy val play = new SimpleModeLayer("play") {
    ext.transport.isPlaying.markInterested()
    ext.transport.isFillModeActive.markInterested()
    ext.transport.isArrangerRecordEnabled.markInterested()
    ext.transport.isAutomationOverrideActive.markInterested()
    ext.transport.isArrangerAutomationWriteEnabled.markInterested()

    val playPressAction: HardwareActionBindable = action(s"$name play pressed", () => {
      val isPlaying = ext.transport.isPlaying
      val t         = ext.transport
      (isPlaying.get(), j.Mod.Shift.btn.isPressed()) match {
        // just play
        case (true, false) => t.play()
        // restart (and stop)
        case (true, true) => t.restart()
        // resume
        case (false, false) => t.continuePlayback()
        case (false, true)  => restart(false)
      }
    })

    def restart(go: Boolean): Unit = {
      val h = ext.host
      ext.transport.stop()
      h.scheduleTask(() => {
        ext.transport.stop()
        h.scheduleTask(() => {
          ext.transport.stop()
          if (go) h.scheduleTask(() => ext.transport.play(), 10)
        }, 10)
      }, 10)
    }

    override val modeBindings = Vector(
      HB(j.play.btn.pressedAction, "play pressed", playPressAction, tracked = false),
      SupBooleanB(j.play.light.isOn, ext.transport.isPlaying),
      HB(j.noteRepeat.btn.pressedAction, "note repeat pressed", () => ext.transport.isFillModeActive.set(true), tracked = false),
      HB(j.noteRepeat.btn.releasedAction, "note repeat released", () => ext.transport.isFillModeActive.set(false), tracked = false),
      SupBooleanB(j.noteRepeat.light.isOn, ext.transport.isFillModeActive),
      HB(j.record.btn.pressedAction, "record pressed", ext.transport.recordAction()),
      SupBooleanB(j.record.light.isOn, ext.transport.isArrangerRecordEnabled),

      HB(j.auto.btn.pressedAction, "auto pressed", () =>
        if (ext.transport.isAutomationOverrideActive.get())
          ext.transport.resetAutomationOverrides()
        else
          ext.transport.isArrangerAutomationWriteEnabled.toggle()
      ),
      SupBooleanB(j.auto.light.isOn, () =>
        if (ext.transport.isAutomationOverrideActive.get())
          j.Mod.blink
        else
          ext.transport.isArrangerAutomationWriteEnabled.get()
      )
    )
  }

  lazy val shiftTransport = new ModeButtonLayer("shiftTransport", j.Mod.Shift, GateMode.Gate) {
    val loop   : SettableBooleanValue = ext.transport.isArrangerLoopEnabled
    val overdub: SettableBooleanValue = ext.transport.isClipLauncherOverdubEnabled
    val metro                         = ext.transport.isMetronomeEnabled
    val auto                          = ext.transport.isClipLauncherAutomationWriteEnabled
    loop.markInterested()
    overdub.markInterested()
    metro.markInterested()
    auto.markInterested()

    def b(button: OnOffButton, name: String, param: SettableBooleanValue) = Vector(
      HB(button.btn.pressedAction, s"shiftTransport $name pressed", () => param.toggle()),
      SupBooleanB(button.light.isOn, param)
    )

    override val modeBindings = Vector(
      b(j.right, "loop", loop),
      b(j.record, "record", overdub),
      b(j.left, "metro", metro),
      b(j.auto, "auto", auto)
    ).flatten ++ Vector(
      HB(j.tempo.btn.pressedAction, "tap tempo", ext.transport.tapTempoAction(),
        // not exclusive so that tap tempo doesn't mess with tempo layer
        tracked = false, behavior = BindingBehavior(exclusive = false))
    )
  }

  lazy val globalQuant = new ModeButtonLayer("globalQuant",
    modeButton = j.grid,
    gateMode = GateMode.Gate
  ) {
    val quant = ext.transport.defaultLaunchQuantization()
    quant.markInterested()
    val enumValues = Vector("8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16")
    override val modeBindings = j.sceneButtons.indices.flatMap { idx =>
      val sceneButton = j.sceneButtons(idx)

      Vector(
        SupColorB(sceneButton.light, () =>
          if (quant.get() == enumValues(idx)) Color.whiteColor() else Color.blackColor()),
        HB(sceneButton.btn.pressedAction, "global quant grid", action(s"grid $idx", () => {
          if (quant.get == enumValues(idx))
            quant.set("none")
          else
            quant.set(enumValues(idx))
        })))
    }
  }

  def buttonGroupChannelMode(
    name: String,
    modeButton: Button,
    group: Seq[JamRgbButton],
    prop: Channel => SettableBooleanValue,
    color: Int // Jam's color index
  ): ModeButtonLayer = ModeButtonLayer(name, modeButton, group.indices.flatMap { idx =>
    val track       = trackBank.getItemAt(idx)
    val propValue   = prop(track)
    val existsValue = track.exists()
    propValue.markInterested()
    existsValue.markInterested()
    val gButton = group(idx)

    Vector(
      HB(gButton.btn.pressedAction, s"group $idx pressed: $name", () => propValue.toggle()),
      SupColorStateB(gButton.light, () => {
        (existsValue.get(), propValue.get()) match {
          case (false, _) => JamColorState.empty
          case (_, false) => JamColorState(color, 0)
          case (_, true)  => JamColorState(color, 3)
        }
      }, JamColorState.empty)
    )
  }
  )

  lazy val solo = buttonGroupChannelMode("solo", j.only(j.solo), j.groupButtons, _.solo(), JamColorBase.YELLOW)
  lazy val mute = buttonGroupChannelMode("mute", j.mute, j.groupButtons, _.mute(), JamColorBase.ORANGE)
}
