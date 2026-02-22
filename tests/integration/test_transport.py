"""Tier 1 — Transport control tests.

Works with any Bitwig project state.
"""

import time

import pytest

from jam_midi import PLAY, RECORD


class TestTransport:

    def test_play_button(self, harness, log):
        """Pressing PLAY starts playback."""
        harness.press(PLAY)

        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")
        assert msg["state"] == "playing"

        log.wait_for(r"KeyMaster eval BtnPlay Press")

    def test_play_stop_toggle(self, harness):
        """Pressing PLAY twice toggles playback on then off."""
        harness.press(PLAY)
        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")
        assert msg["state"] == "playing"

        time.sleep(0.3)

        harness.press(PLAY)
        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "stopped")
        assert msg["state"] == "stopped"

    def test_record_button(self, harness, log):
        """Pressing RECORD then PLAY starts recording."""
        # RECORD from stopped arms recording; need PLAY to actually start
        harness.press(RECORD)
        log.wait_for(r"KeyMaster eval BtnRecord Press")

        harness.press(PLAY)
        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "recording")
        assert msg["state"] == "recording"

    def test_direct_transport_play(self, harness):
        """Sending /transport/play directly starts playback (bypasses MIDI)."""
        harness.send_command("/transport/play")

        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")
        assert msg["state"] == "playing"

    def test_direct_transport_stop(self, harness):
        """Sending /transport/stop directly stops playback."""
        # Start first
        harness.send_command("/transport/play")
        harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")

        time.sleep(0.2)

        harness.send_command("/transport/stop")
        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "stopped")
        assert msg["state"] == "stopped"
