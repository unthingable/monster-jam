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

    Note: the harness track bank is a top-level-only createMainTrackBank
    and does not follow MonsterJam's group navigation — we cannot verify
    navigation state through it.  Use a generous settle time instead.
    """
    for _ in range(3):
        with harness.holding(GROUP[0]):
            harness.press(DPAD_UP)
            time.sleep(0.1)
        time.sleep(0.1)
    harness.drain(timeout=0.1)


def _enter_group(harness, group_idx):
    """Navigate into a group track by holding GROUP[idx]+DPAD_DOWN.

    Note: the harness track bank is top-level-only and cannot observe
    group navigation.  We settle with a time-based wait instead.
    """
    with harness.holding(GROUP[group_idx]):
        harness.press(DPAD_DOWN)
        time.sleep(0.1)
    # Settle time for MonsterJam to process navigation (no observable state)
    time.sleep(0.5)
    harness.drain(timeout=0.1)


class TestGroupClip:
    """S6.5: On a group track, pressing a clip pad launches the scene."""

    @pytest.fixture(autouse=True)
    def at_top_level(self, harness):
        _navigate_to_top(harness)
        yield
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

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

    def test_scene_at_top_launches_globally(self, harness):
        """At top level, pressing SCENE[0] still performs a global scene launch."""
        harness.press(SCENE[0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1


class TestTrackGate:
    """Track gate: hold GROUP to access DPAD navigation in/out of groups."""

    @pytest.fixture(autouse=True)
    def at_top_level(self, harness):
        _navigate_to_top(harness)
        yield
        _navigate_to_top(harness)

    def test_group_hold_enters_group_with_dpad_down(self, harness, jam):
        """Hold GROUP[n] + press DPAD_DOWN enters the group track.

        Verified by observing that the GROUP button LEDs are repainted
        after navigation — inside the group, the LEDs reflect child
        tracks instead of top-level tracks.  Direct clip observation
        inside the group is not possible because the harness track bank
        is top-level-only.
        """
        group_idx = _find_group_track(harness)
        if group_idx is None:
            pytest.skip("No group track found in current bank")

        harness.drain(timeout=0.1)

        # Navigate into the group (inline, without draining, so we can
        # observe the LED repaint burst).
        with harness.holding(GROUP[group_idx]):
            harness.press(DPAD_DOWN)
            time.sleep(0.1)
        time.sleep(0.5)

        # Entering a group triggers a surface repaint.  The GROUP button
        # LEDs (NoteOn ch1, notes 8-15) are updated to reflect child
        # tracks.  Collect MIDI that arrived during navigation.
        # Filter out the held button itself (note 8+group_idx) to avoid
        # false positives from the hold/release LED feedback.
        held_note = GROUP[group_idx][1]
        midi = harness.collect_midi(timeout=0.5)
        group_leds = [
            m for m in midi
            if m.get("status") == 0x90
            and m.get("channel") == 1
            and 8 <= m.get("data1", -1) <= 15
            and m.get("data1") != held_note
        ]

        # MonsterJam repaints all 8 GROUP LEDs when navigating into a
        # group, so we expect updates on buttons other than the held one.
        assert len(group_leds) > 0, (
            "No GROUP LED updates observed after entering group "
            f"(excluding held button note={held_note}) — "
            "navigation may not have occurred"
        )

    def test_group_hold_exits_group_with_dpad_up(self, harness, jam):
        """Hold GROUP[n] + DPAD_DOWN enters, then DPAD_UP exits the group."""
        group_idx = _find_group_track(harness)
        if group_idx is None:
            pytest.skip("No group track found in current bank")

        _enter_group(harness, group_idx)

        # Exit via DPAD_UP
        with harness.holding(GROUP[group_idx]):
            harness.press(DPAD_UP)
            time.sleep(0.1)
        # Settle for navigation (no observable state)
        time.sleep(0.5)
        harness.drain(timeout=0.1)

        # Verify we're back at top level by checking that pressing the
        # group track's pad launches a scene (top-level group behavior)
        harness.press(PAD[0][group_idx])
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
