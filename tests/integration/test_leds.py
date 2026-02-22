"""Tier 1 — LED feedback tests.

Verifies that MonsterJam sends correct MIDI messages back to the hardware
to update button LEDs in response to state changes.

The harness MIDI proxy forwards MonsterJam's MIDI output as /midi/in OSC
messages with args: channel, status, data1, data2.
"""

import time

import pytest

from jam_midi import PLAY, LEVEL, AUX, GROUP

# MIDI status bytes (without channel)
CC = 0xB0    # 176
NOTE_ON = 0x90  # 144


def _is_cc(data1, msg):
    """Check if a /midi/in message is a CC for the given data1 number."""
    return msg.get("status") == CC and msg.get("data1") == data1


def _is_note(data1, msg):
    """Check if a /midi/in message is a NoteOn for the given note number."""
    return msg.get("status") == NOTE_ON and msg.get("data1") == data1


class TestLeds:

    def test_play_led_on(self, harness):
        """Pressing PLAY lights up the Play button LED."""
        harness.press(PLAY)
        harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")

        msg = harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(PLAY[1], m) and m.get("data2", 0) > 0,
            timeout=2.0,
        )
        assert msg["data2"] > 0

    def test_play_led_off(self, harness):
        """Stopping transport turns off the Play button LED."""
        harness.press(PLAY)
        harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")
        harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(PLAY[1], m) and m.get("data2", 0) > 0,
        )

        time.sleep(0.3)
        harness.drain(timeout=0.1)

        harness.press(PLAY)
        harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "stopped")

        msg = harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(PLAY[1], m) and m.get("data2", 0) == 0,
            timeout=2.0,
        )
        assert msg["data2"] == 0

    def test_level_led_on(self, harness):
        """Activating LEVEL mode lights up the Level button LED."""
        # Ensure LEVEL is off first by activating a different strip mode
        harness.press(AUX)
        time.sleep(0.3)
        harness.drain(timeout=0.2)

        harness.press(LEVEL)

        msg = harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(LEVEL[1], m) and m.get("data2", 0) > 0,
            timeout=2.0,
        )
        assert msg["data2"] > 0

    def test_aux_replaces_level_led(self, harness):
        """Switching from LEVEL to AUX turns off Level LED and lights Aux LED."""
        # Ensure LEVEL is active
        harness.press(AUX)
        time.sleep(0.3)
        harness.press(LEVEL)
        harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(LEVEL[1], m) and m.get("data2", 0) > 0,
        )
        time.sleep(0.3)
        harness.drain(timeout=0.1)

        # Switch to AUX — collect ALL MIDI from the burst
        harness.press(AUX)
        midi = harness.collect_midi(timeout=1.5)

        aux_on = [m for m in midi if _is_cc(AUX[1], m) and m["data2"] > 0]
        level_off = [m for m in midi if _is_cc(LEVEL[1], m) and m["data2"] == 0]

        assert aux_on, f"Expected AUX LED on, got: {[m for m in midi if _is_cc(AUX[1], m)]}"
        assert level_off, f"Expected LEVEL LED off, got: {[m for m in midi if _is_cc(LEVEL[1], m)]}"

    def test_group_led_updates_on_select(self, harness):
        """Selecting a different track updates group button LED color."""
        # Select group B first to force a state change on group A
        harness.press(GROUP[1])
        time.sleep(0.3)
        harness.drain(timeout=0.2)

        group_a_note = GROUP[0][1]  # note number = 8
        harness.press(GROUP[0])

        # Group LEDs are NoteOn on channel 1 with a color+brightness value
        msg = harness.wait_for(
            "/midi/in",
            predicate=lambda m: (
                _is_note(group_a_note, m)
                and m.get("channel") == 1
                and m.get("data2", 0) > 0
            ),
            timeout=2.0,
        )
        assert msg["data2"] > 0  # non-zero = some color at some brightness
