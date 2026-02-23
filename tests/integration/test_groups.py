"""Tier 2 — Group-aware clip and scene launch tests.

Requires a known Bitwig project with:
- At least one group track in the first bank (positions 0-7)
- The group must contain tracks with clips in the first scene (row 0)
- At least one non-group track (outside the group) with a clip in scene 0

Tests:
- S6.5: Pressing a clip pad on a group track launches the scene
- S6.6: Inside a group, SCENE buttons launch clips per-track

Mark: known_project
"""

import time

import pytest

from jam_midi import DPAD_DOWN, DPAD_UP, GROUP, PAD, SCENE

pytestmark = pytest.mark.known_project


def _find_group_track(harness):
    """Return bank_index of the first group track in the current bank, or None."""
    state = harness.last_state
    for i in range(8):
        key = f"/state/track/{i}"
        msg = state.get(key, {})
        if msg.get("type") == "Group":
            return i
    return None


def _navigate_to_top(harness):
    """Navigate to top level by holding GROUP[0]+DPAD_UP several times.

    At top level, DPAD_UP is a no-op, so this is always safe.
    """
    for _ in range(3):
        harness.hold(GROUP[0])
        time.sleep(0.2)
        harness.press(DPAD_UP)
        time.sleep(0.3)
        harness.release(GROUP[0])
        time.sleep(0.3)
    harness.drain(timeout=0.3)


def _enter_group(harness, group_idx):
    """Navigate into a group track by holding GROUP[idx]+DPAD_DOWN."""
    harness.hold(GROUP[group_idx])
    time.sleep(0.3)
    harness.press(DPAD_DOWN)
    time.sleep(0.5)
    harness.release(GROUP[group_idx])
    time.sleep(0.5)
    harness.drain(timeout=0.3)


class TestGroupClip:
    """S6.5: On a group track, pressing a clip pad launches the scene."""

    @pytest.fixture(autouse=True)
    def at_top_level(self, harness):
        _navigate_to_top(harness)

    def test_group_pad_launches_scene(self, harness):
        """Pressing a pad on a group track column launches the scene at that row."""
        group_idx = _find_group_track(harness)
        if group_idx is None:
            pytest.skip("No group track found in current bank")

        harness.press(PAD[0][group_idx])

        # Scene launch fires clips across all tracks in that row.
        # Verify a clip plays on a non-group track.
        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: (
                m.get("scene") == 0
                and m.get("is_playing") == 1
                and m.get("track") != group_idx
            ),
            timeout=3.0,
        )
        assert msg["is_playing"] == 1


class TestGroupScene:
    """S6.6: Inside a group, SCENE buttons launch clips per-track."""

    @pytest.fixture(autouse=True)
    def at_top_level(self, harness):
        _navigate_to_top(harness)
        yield
        _navigate_to_top(harness)

    def test_scene_inside_group_launches_clips(self, harness):
        """Inside a group, pressing SCENE[0] launches clips per-track."""
        group_idx = _find_group_track(harness)
        if group_idx is None:
            pytest.skip("No group track found in current bank")

        _enter_group(harness, group_idx)

        harness.press(SCENE[0])

        # Group scene launch fires clipLauncherSlotBank().launch(0) for
        # each track in the bank.  Verify at least one clip starts playing.
        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

    def test_scene_at_top_launches_globally(self, harness):
        """At top level, pressing SCENE[0] still performs a global scene launch."""
        # Sanity check: normal scene behavior at top level (isAtTop=true)
        harness.press(SCENE[0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1
