package com.github.unthingable.util;

import com.bitwig.extension.controller.api.MidiOut;

/**
 * Wraps a primary MidiOut port and optionally mirrors all MIDI output to a second port.
 * When no mirror is set, this is a thin passthrough with a null check per call.
 */
public class MirroringMidiOut implements MidiOut {
    private final MidiOut primary;
    private MidiOut mirror;

    public MirroringMidiOut(MidiOut primary) {
        this.primary = primary;
    }

    public void setMirror(MidiOut mirror) {
        this.mirror = mirror;
    }

    @Override
    public void sendMidi(int status, int data1, int data2) {
        primary.sendMidi(status, data1, data2);
        if (mirror != null) mirror.sendMidi(status, data1, data2);
    }

    @Override
    public void sendSysex(String hexString) {
        primary.sendSysex(hexString);
        if (mirror != null) mirror.sendSysex(hexString);
    }

    @Override
    public void sendSysex(byte[] data) {
        primary.sendSysex(data);
        if (mirror != null) mirror.sendSysex(data);
    }

    @Override
    public void sendSysexBytes(byte[] data) {
        primary.sendSysexBytes(data);
        if (mirror != null) mirror.sendSysexBytes(data);
    }

    @Override
    @SuppressWarnings("deprecation")
    public void setShouldSendMidiBeatClock(boolean shouldSendClock) {
        primary.setShouldSendMidiBeatClock(shouldSendClock);
    }
}
