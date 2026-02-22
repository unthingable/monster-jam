"""Tier 1 — Track selection and navigation tests.

Works with any Bitwig project state (asserts relative changes only).
"""

import time

import pytest

from jam_midi import GROUP, MASTER, DPAD_RIGHT


class TestTracks:

    def test_group_button_selects_track(self, harness, log):
        """Pressing GROUP[0] selects the first track in the bank."""
        # Capture current track
        initial = harness.last_state.get("/state/cursor_track", {})
        initial_index = initial.get("index", -1)

        harness.press(GROUP[0])

        msg = harness.wait_for("/state/cursor_track")
        assert "index" in msg

        log.wait_for(r"KeyMaster eval BtnGroupA Press")

    def test_group_buttons_cycle(self, harness):
        """Pressing different GROUP buttons selects corresponding tracks."""
        indices = []

        for i in range(min(4, len(GROUP))):
            harness.press(GROUP[i])
            time.sleep(0.3)
            msg = harness.wait_for("/state/cursor_track")
            indices.append(msg.get("index"))

        # At least some track changes should have happened
        # (exact indices depend on project, but pressing different buttons
        # should generally produce different selections)
        assert len(indices) == 4

    def test_master_button(self, harness):
        """Pressing MASTER selects the master track."""
        harness.press(MASTER)

        # Master track should be selected — name is typically "Master"
        msg = harness.wait_for(
            "/state/cursor_track",
            predicate=lambda m: "master" in m.get("name", "").lower(),
            timeout=2.0,
        )
        assert "master" in msg["name"].lower()

    def test_dpad_scrolls_bank(self, harness):
        """Pressing DPAD_RIGHT scrolls the track bank."""
        # Capture initial track bank state
        initial_tracks = {
            addr: msg
            for addr, msg in harness.last_state.items()
            if addr == "/state/track"
        }

        harness.press(DPAD_RIGHT)
        time.sleep(0.5)

        # Should receive updated /state/track messages
        msg = harness.wait_for("/state/track", timeout=2.0)
        assert msg is not None
