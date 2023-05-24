package com.github.unthingable.jam.layer

import com.bitwig.extension.api.Color
import com.bitwig.extension.controller.api.SettableBooleanValue
import com.bitwig.extension.controller.api.Track
import com.github.unthingable.JamSettings
import com.github.unthingable.Util
import com.github.unthingable.framework.binding.BindingDSL
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.HB
import com.github.unthingable.framework.binding.HB.action
import com.github.unthingable.framework.binding.SupBooleanB
import com.github.unthingable.framework.binding.SupColorB
import com.github.unthingable.framework.binding.SupColorStateB
import com.github.unthingable.framework.binding.BindingBehavior as BB
import com.github.unthingable.framework.mode.GateMode
import com.github.unthingable.framework.mode.ModeButtonLayer
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam
import com.github.unthingable.jam.surface.JamColor.JamColorBase
import com.github.unthingable.jam.surface.JamColorState
import com.github.unthingable.jam.surface.JamOnOffButton
import com.github.unthingable.jam.surface.JamRgbButton

import java.time.Instant
import scala.concurrent.duration.*

trait TransportL extends BindingDSL, Util:
  this: Jam =>
  lazy val position = SimpleModeLayer(
    "position",
    Vector(
      HB(
        j.encoder.turn,
        "position turn",
        ext.host.createRelativeHardwareControlStepTarget(
          ext.transport.fastForwardAction(),
          ext.transport.rewindAction()
        )
      )
    )
  )

  lazy val tempoLayer = ModeButtonLayer(
    "tempo",
    j.tempo,
    Vector(
      HB(
        j.encoder.turn,
        "tempo turn",
        stepTarget(
          () => ext.transport.increaseTempo(1, 647 * (if j.Mod.Shift.btn.isPressed then 20 else 1)),
          () => ext.transport.increaseTempo(-1, 647 * (if j.Mod.Shift.btn.isPressed then 20 else 1))
        )
      )
    )
  )

  lazy val play = new SimpleModeLayer("play"):
    ext.transport.isPlaying.markInterested()
    ext.transport.isFillModeActive.markInterested()
    ext.transport.isArrangerRecordEnabled.markInterested()
    ext.transport.isAutomationOverrideActive.markInterested()
    ext.transport.isArrangerAutomationWriteEnabled.markInterested()

    inline def shiftResume = ext.preferences.shiftPlay.get() == JamSettings.ShiftPlay.`Pause/Resume`

    var playPressed: Option[Instant] = None

    def playPress(): Unit =
      val now              = Instant.now
      lazy val isDoubleTap = playPressed.exists(_.isAfter(now.minusMillis(200)))
      val isPlaying        = ext.transport.isPlaying
      val t                = ext.transport
      (isPlaying.get(), j.Mod.Shift.btn.isPressed) match
        case (true, _) if isDoubleTap => restart(true)
        case (true, true)             => if shiftResume then t.stop() else restart(true)
        case (true, false)            => t.stop()
        case (false, false)           => t.play()
        case (false, true)            => if shiftResume then t.continuePlayback() else restart(true)
      playPressed = Some(now)

    def restart(go: Boolean): Unit =
      val h = ext.host
      ext.transport.stop()
      h.scheduleTask(
        () =>
          ext.transport.stop()
          // h.scheduleTask(
          //   () => {
          //     ext.transport.stop()
          if go then h.scheduleTask(() => ext.transport.play(), 10)
        ,
        30
      )
      // },
      // 10
      // )

    override val modeBindings = Vector(
      // HB(j.play.btn.pressed, "play pressed", playPressAction, BB(tracked = false)),
      EB(j.play.st.press, "", () => playPress(), BB(tracked = false)),
      SupBooleanB(j.play.light, ext.transport.isPlaying),
      EB(
        j.noteRepeat.st.press,
        "note repeat pressed",
        () => ext.transport.isFillModeActive.set(true),
        BB(tracked = false)
      ),
      EB(
        j.noteRepeat.st.release,
        "note repeat released",
        () => ext.transport.isFillModeActive.set(false),
        BB(tracked = false)
      ),
      SupBooleanB(j.noteRepeat.light, ext.transport.isFillModeActive),
      EB(
        j.record.st.release,
        "record released",
        () => if !record.isOlderThan(500.millis) && !record.hasDirtyBindings() then ext.transport.record(),
        BB.omni
      ),
      SupBooleanB(j.record.light, ext.transport.isArrangerRecordEnabled),
      EB(
        j.auto.st.press,
        "auto pressed",
        () =>
          if ext.transport.isAutomationOverrideActive.get() then ext.transport.resetAutomationOverrides()
          else ext.transport.isArrangerAutomationWriteEnabled.toggle()
      ),
      SupBooleanB(
        j.auto.light,
        () =>
          if ext.transport.isAutomationOverrideActive.get() then j.Mod.blink
          else ext.transport.isArrangerAutomationWriteEnabled.get()
      )
    )

  lazy val shiftTransport = new ModeButtonLayer("shiftTransport", j.Mod.Shift, GateMode.Gate):
    val loop: SettableBooleanValue    = ext.transport.isArrangerLoopEnabled
    val overdub: SettableBooleanValue = ext.transport.isClipLauncherOverdubEnabled
    val metro                         = ext.transport.isMetronomeEnabled
    val auto                          = ext.transport.isClipLauncherAutomationWriteEnabled

    loop.markInterested()
    overdub.markInterested()
    metro.markInterested()
    auto.markInterested()

    // import reflect.Selectable.reflectiveSelectable
    def b(button: JamOnOffButton, name: String, param: SettableBooleanValue) = Vector(
      // FIXME - fixed?
      EB(button.st.press, s"shiftTransport $name pressed", () => param.toggle()),
      SupBooleanB(button.light, param)
    )

    override val modeBindings = Vector(
      b(j.right, "loop", loop),
      b(j.record, "record", overdub),
      b(j.left, "metro", metro),
      b(j.auto, "auto", auto)
    ).flatten

  lazy val shiftTempo = SimpleModeLayer(
    "shiftTempo",
    Vector(
      EB(
        j.Combo.Shift.tempo.press,
        "tap tempo",
        () => ext.transport.tapTempo(),
      )
    )
  )

  lazy val globalQuant =
    new ModeButtonLayer("globalQuant", modeButton = j.grid, gateMode = GateMode.Gate):
      val quant = ext.transport.defaultLaunchQuantization()
      quant.markInterested()
      val enumValues = Vector("8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16")
      override val modeBindings = j.sceneButtons.indices.flatMap { idx =>
        val sceneButton = j.sceneButtons(idx)

        Vector(
          SupColorB(
            sceneButton.light,
            () => if quant.get() == enumValues(idx) then Color.whiteColor() else Color.blackColor()
          ),
          EB(
            sceneButton.st.press,
            "global quant grid",
            () =>
              if quant.get == enumValues(idx) then quant.set("none")
              else quant.set(enumValues(idx))
          )
        )
      }

  def buttonGroupChannelMode(
    name: String,
    modeButton: JamOnOffButton,
    group: Seq[JamRgbButton],
    prop: Track => SettableBooleanValue,
    color: Int, // Jam's color index
    gateMode: GateMode = GateMode.Auto,
    silent: Boolean = false
  ): ModeButtonLayer = ModeButtonLayer(
    name,
    modeButton,
    group.indices.flatMap { idx =>
      val track       = trackBank.getItemAt(idx)
      val propValue   = prop(track)
      val existsValue = track.exists()
      val gButton     = group(idx)

      propValue.markInterested()
      existsValue.markInterested()

      Vector(
        EB(gButton.st.press, s"group $idx pressed: $name", () => propValue.toggle()),
        SupColorStateB(
          gButton.light,
          () =>
            (existsValue.get(), propValue.get()) match
              case (false, _) => JamColorState.empty
              case (_, false) => JamColorState(color, 0)
              case (_, true)  => JamColorState(color, 3)
          ,
          JamColorState.empty
        )
      )
    } ++
      Vector(
        EB(j.clear.st.press, "", () => superBank.itemView.map(prop).foreach(_.set(false)))
      ),
    gateMode = gateMode,
    silent = silent
  )

  lazy val solo =
    buttonGroupChannelMode("solo", j.solo, j.groupButtons, _.solo(), JamColorBase.YELLOW)
  lazy val mute =
    buttonGroupChannelMode("mute", j.mute, j.groupButtons, _.mute(), JamColorBase.ORANGE)
  lazy val record =
    buttonGroupChannelMode("record", j.record, j.groupButtons, _.arm(), JamColorBase.RED, GateMode.Gate, true)
end TransportL
