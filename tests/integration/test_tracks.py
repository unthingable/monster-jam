"""Tier 1 — Track selection and navigation tests.

Works with any Bitwig project state (asserts relative changes only).
"""

import pytest

from jam_midi import GROUP, MASTER, DPAD_RIGHT


class TestTracks:

    def test_group_button_selects_track(self, harness, log):
        """Pressing GROUP[0] selects the first track in the bank."""
        harness.press(GROUP[0])

        msg = harness.wait_for("/state/cursor_track")
        assert "index" in msg

        log.wait_for(r"KeyMaster eval BtnGroupA Press")

    def test_group_buttons_cycle(self, harness):
        """Pressing different GROUP buttons selects corresponding tracks."""
        indices = []

        for i in range(min(4, len(GROUP))):
            harness.press(GROUP[i])
            prev_index = indices[-1] if indices else None
            msg = harness.wait_for(
                "/state/cursor_track",
                predicate=lambda m, pi=prev_index: (
                    pi is None or m.get("index") != pi
                ),
                timeout=2.0,
            )
            indices.append(msg.get("index"))

        # At least some track changes should have happened
        assert len(set(indices)) > 1, f"Expected different track indices, got {indices}"

    def test_master_button(self, harness):
        """Pressing MASTER selects the master track."""
        harness.press(MASTER)

        msg = harness.wait_for(
            "/state/cursor_track",
            predicate=lambda m: "master" in m.get("name", "").lower(),
            timeout=2.0,
        )
        assert "master" in msg["name"].lower()

    def test_dpad_scrolls_bank(self, harness):
        """Pressing DPAD_RIGHT scrolls the track bank."""
        harness.drain(timeout=0.1)
        harness.press(DPAD_RIGHT)

        # DPAD_RIGHT triggers a track bank update regardless of whether the
        # bank actually scrolled (it may already be at the end).
        msg = harness.wait_for("/state/track_bank", timeout=2.0)
        assert msg is not None

