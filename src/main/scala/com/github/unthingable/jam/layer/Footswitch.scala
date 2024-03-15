package com.github.unthingable.jam.layer

import com.bitwig.extension.controller.api.ClipLauncherSlot
import com.bitwig.extension.controller.api.ClipLauncherSlotBank
import com.bitwig.extension.controller.api.Track
import com.github.unthingable.JamSettings.EnumSetting
import com.github.unthingable.JamSettings.SwitchRecord
import com.github.unthingable.framework.binding.Binding
import com.github.unthingable.framework.binding.EB
import com.github.unthingable.framework.binding.GlobalEvent.SlotSelected
import com.github.unthingable.framework.mode.SimpleModeLayer
import com.github.unthingable.jam.Jam

trait Footswitch:
  this: Jam =>
  lazy val footswitch = new SimpleModeLayer("footswitch"):
    type SlotWithBank = (ClipLauncherSlot, ClipLauncherSlotBank)
    var selectedSlot: Option[SlotWithBank] = None

    ext.events.addSub: (e: SlotSelected) =>
      val track: Track           = superBank.getItemAt(e.globalTrack)
      val bank                   = track.clipLauncherSlotBank()
      val slot: ClipLauncherSlot = bank.getItemAt(e.globalSlot)
      selectedSlot = Some((slot, bank))

    def handlePress(pref: EnumSetting[SwitchRecord]): Unit =
      pref
        .get()
        .match
          case SwitchRecord.`Global Record` => ext.transport.record()
          case SwitchRecord.`Current Clip` =>
            selectedSlot.foreach: (slot, bank) =>
              if slot.isPlaying().get() then bank.stop()
              else slot.launch()

    override def modeBindings: Seq[Binding[?, ?, ?]] = Seq(
      EB(j.pedalTip.st.press, "pedal tip press", () => handlePress(ext.preferences.fswTip)),
      EB(j.pedalRing.st.press, "pedal ring press", () => handlePress(ext.preferences.fswRing))
    )
end Footswitch
