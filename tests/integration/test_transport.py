"""Tier 1 — Transport control tests.

Works with any Bitwig project state.
"""

import time

import pytest

from jam_midi import CLEAR, GRID, GROUP, MUTE, PLAY, RECORD, SCENE, SOLO


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
        harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")

        harness.press(PLAY)
        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "stopped")
        assert msg["state"] == "stopped"

    def test_record_button(self, harness, log):
        """Pressing RECORD then PLAY starts recording."""
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
        harness.send_command("/transport/play")
        harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "playing")

        harness.send_command("/transport/stop")
        msg = harness.wait_for("/state/transport", predicate=lambda m: m["state"] == "stopped")
        assert msg["state"] == "stopped"

    def test_solo_mode_activates(self, harness, jam):
        """Pressing SOLO activates solo mode; pressing again deactivates it."""
        harness.press(SOLO)
        jam.assert_mode_active("solo")

        harness.press(SOLO)
        jam.assert_mode_inactive("solo")

    def test_mute_mode_activates(self, harness, jam):
        """Pressing MUTE activates mute mode; pressing again deactivates it."""
        harness.press(MUTE)
        jam.assert_mode_active("mute")

        harness.press(MUTE)
        jam.assert_mode_inactive("mute")

    def test_solo_mute_exclusive(self, harness, jam):
        """SOLO and MUTE are mutually exclusive — activating one deactivates the other."""
        harness.press(SOLO)
        jam.assert_mode_active("solo")

        harness.press(MUTE)
        jam.assert_mode_active("mute")
        jam.assert_mode_inactive("solo")

    def test_solo_toggles_track(self, harness, jam):
        """Pressing GROUP[0] while in SOLO mode toggles track 0 solo state."""
        harness.press(SOLO)
        jam.assert_mode_active("solo")

        harness.press(GROUP[0])
        time.sleep(0.1)
        # Press again to restore — solo state should have changed and changed back
        harness.press(GROUP[0])

        # Solo mode remains active after track presses
        jam.assert_mode_active("solo")

    def test_mute_toggles_track(self, harness, jam):
        """Pressing GROUP[0] while in MUTE mode toggles track 0 mute state."""
        harness.press(MUTE)
        jam.assert_mode_active("mute")

        harness.press(GROUP[0])
        time.sleep(0.1)
        # Press again to restore — mute state should have changed and changed back
        harness.press(GROUP[0])

        # Mute mode remains active after track presses
        jam.assert_mode_active("mute")


class TestAutoGating:
    """Auto-gating behavior: quick press toggles, long press gates, usage tracks."""

    def test_solo_long_press_exits_on_release(self, harness, jam):
        """Hold SOLO >500ms without pressing any GROUP — exits on release."""
        with harness.holding(SOLO):
            time.sleep(0.6)  # must exceed 500ms threshold
            jam.assert_mode_active("solo")
        jam.assert_mode_inactive("solo")

    def test_mute_long_press_exits_on_release(self, harness, jam):
        """Hold MUTE >500ms without pressing any GROUP — exits on release."""
        with harness.holding(MUTE):
            time.sleep(0.6)  # must exceed 500ms threshold
            jam.assert_mode_active("mute")
        jam.assert_mode_inactive("mute")

    def test_mute_long_press_with_usage_exits(self, harness, jam):
        """Hold MUTE, press GROUP (usage), release MUTE — exits."""
        with harness.holding(MUTE):
            jam.assert_mode_active("mute")
            harness.press(GROUP[0])
            time.sleep(0.1)
            # Undo the mute toggle so test is idempotent
            harness.press(GROUP[0])
        jam.assert_mode_inactive("mute")

    def test_solo_long_press_with_usage_exits(self, harness, jam):
        """Hold SOLO, press GROUP (usage), release SOLO — exits."""
        with harness.holding(SOLO):
            jam.assert_mode_active("solo")
            harness.press(GROUP[0])
            time.sleep(0.1)
            # Undo the solo toggle so test is idempotent
            harness.press(GROUP[0])
        jam.assert_mode_inactive("solo")

    def test_solo_quick_press_toggles_on(self, harness, jam):
        """Quick press (<500ms) SOLO with no usage — stays active (toggle on)."""
        harness.press(SOLO)
        jam.assert_mode_active("solo")
        # Clean up
        harness.press(SOLO)
        jam.assert_mode_inactive("solo")

    def test_solo_quick_press_toggles_off(self, harness, jam):
        """Quick press SOLO twice — first toggles on, second toggles off."""
        harness.press(SOLO)
        jam.assert_mode_active("solo")
        harness.press(SOLO)
        jam.assert_mode_inactive("solo")

    def test_clear_in_solo_unsolo_all(self, harness, jam):
        """Pressing CLEAR while in SOLO mode unsolos all tracks."""
        harness.press(SOLO)
        jam.assert_mode_active("solo")

        # Ensure track 0 starts unsoloed (previous test may have left it soloed)
        track_state = harness.last_state.get("/state/track/0", {})
        if track_state.get("solo", 0) == 1:
            harness.press(GROUP[0])
            harness.wait_for(
                "/state/track",
                predicate=lambda m: m.get("bank_index") == 0 and m.get("solo") == 0,
            )

        # Solo track 0 and wait for the state change to confirm
        harness.press(GROUP[0])
        harness.wait_for(
            "/state/track",
            predicate=lambda m: m.get("bank_index") == 0 and m.get("solo") == 1,
        )

        # CLEAR should unsolo all — verify the state reverts to 0
        harness.press(CLEAR)
        try:
            msg = harness.wait_for(
                "/state/track",
                predicate=lambda m: m.get("bank_index") == 0 and m.get("solo") == 0,
            )
            assert msg["solo"] == 0
        finally:
            # Fallback: if CLEAR failed, track 0 may still be soloed; toggle it off
            state = harness.last_state
            track_state = state.get("/state/track/0", {})
            if track_state.get("solo", 0) != 0:
                harness.press(GROUP[0])
                time.sleep(0.1)

        # Solo mode should still be active after CLEAR
        jam.assert_mode_active("solo")

        # Clean up
        harness.press(SOLO)
        jam.assert_mode_inactive("solo")

    def test_clear_in_mute_unmute_all(self, harness, jam):
        """Pressing CLEAR while in MUTE mode unmutes all tracks."""
        harness.press(MUTE)
        jam.assert_mode_active("mute")

        # Ensure track 0 starts unmuted (previous test may have left it muted)
        track_state = harness.last_state.get("/state/track/0", {})
        if track_state.get("mute", 0) == 1:
            harness.press(GROUP[0])
            harness.wait_for(
                "/state/track",
                predicate=lambda m: m.get("bank_index") == 0 and m.get("mute") == 0,
            )

        # Mute track 0 and wait for the state change to confirm
        harness.press(GROUP[0])
        harness.wait_for(
            "/state/track",
            predicate=lambda m: m.get("bank_index") == 0 and m.get("mute") == 1,
        )

        # CLEAR should unmute all — verify the state reverts to 0
        harness.press(CLEAR)
        try:
            msg = harness.wait_for(
                "/state/track",
                predicate=lambda m: m.get("bank_index") == 0 and m.get("mute") == 0,
            )
            assert msg["mute"] == 0
        finally:
            # Fallback: if CLEAR failed, track 0 may still be muted; toggle it off
            state = harness.last_state
            track_state = state.get("/state/track/0", {})
            if track_state.get("mute", 0) != 0:
                harness.press(GROUP[0])
                time.sleep(0.1)

        # Mute mode should still be active after CLEAR
        jam.assert_mode_active("mute")

        # Clean up
        harness.press(MUTE)
        jam.assert_mode_inactive("mute")


class TestGlobalQuant:
    """GRID button gates the global quantization selector."""

    def test_grid_is_gate_mode(self, harness, jam):
        """GRID mode is a gate — active while held, deactivates on release."""
        with harness.holding(GRID):
            jam.assert_mode_active("globalQuant")
        jam.assert_mode_inactive("globalQuant")

    def test_grid_shows_scene_leds(self, harness):
        """Holding GRID sends LED updates to scene buttons to show quant options."""
        harness.drain(timeout=0.1)

        with harness.holding(GRID):
            # Scene buttons are NoteOn on channel 1, notes 0-7
            # When GRID is held, they should show quant options (some lit)
            midi = harness.collect_midi(timeout=0.5)
            scene_msgs = [
                m for m in midi
                if m.get("status") == 0x90 and m.get("channel") == 1
                and 0 <= m.get("data1", -1) <= 7
            ]
            assert len(scene_msgs) > 0, "Expected scene button LED updates while GRID held"
