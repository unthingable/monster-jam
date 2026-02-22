"""Tier 2 — Clip launching tests.

Requires a known Bitwig project with clips in the matrix.

Mark: known_project
"""

import time

import pytest

from jam_midi import PAD

pytestmark = pytest.mark.known_project


class TestClips:

    def test_clip_launch_via_osc(self, harness):
        """Launch clip at track 0, scene 0 via direct OSC command."""
        harness.send_command("/clip/launch", ("i", "0"), ("i", "0"))

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("track") == 0 and m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

    def test_clip_launch_via_midi(self, harness):
        """Launch clip via matrix pad press (MIDI through MonsterJam)."""
        # Press pad [0][0] — top-left matrix button
        harness.press(PAD[0][0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

    def test_scene_launch(self, harness):
        """Launch scene 0 — all clips in scene should start playing."""
        harness.send_command("/scene/launch", ("i", "0"))

        # Wait for at least one clip to start playing
        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1
