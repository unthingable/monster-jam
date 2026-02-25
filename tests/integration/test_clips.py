"""Tier 2 — Clip launching tests.

Requires a known Bitwig project with clips in the matrix.

Mark: known_project
"""

import time

import pytest

from jam_midi import CLEAR, DUPLICATE, PAD, SELECT

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
        harness.press(PAD[0][0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

    def test_group_clip_stop_via_midi(self, harness):
        """Pressing a playing group pad again should stop it, not relaunch scene.

        Regression: group-aware launch (isGroup check) was evaluated before
        the isPlaying stop check, so pressing a playing clip on a group track
        column would re-launch the scene instead of stopping.

        Strategy: launch scene, stop a non-group track, then press the group
        pad. If buggy (scene relaunches), the stopped track starts playing
        again. If fixed (group stops), the stopped track stays stopped.
        """
        group_idx = None
        other_idx = None
        for i in range(8):
            msg = harness.last_state.get(f"/state/track/{i}", {})
            if msg.get("type") == "Group" and group_idx is None:
                group_idx = i
            elif msg.get("type") and other_idx is None:
                other_idx = i
        if group_idx is None:
            pytest.skip("No group track in current bank")
        if other_idx is None:
            pytest.skip("No non-group track in current bank")

        # Launch scene so all tracks are playing
        harness.send_command("/scene/launch", ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("track") == other_idx and m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )

        # Stop the non-group track by pressing its pad (toggle off)
        harness.press(PAD[0][other_idx])
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("track") == other_idx and m.get("scene") == 0 and m.get("is_playing") == 0,
            timeout=3.0,
        )

        # Press the group track's pad — should stop group, NOT relaunch scene
        harness.press(PAD[0][group_idx])
        time.sleep(1.0)

        # If the bug is present the scene relaunched, restarting the stopped
        # track. If fixed, only the group stopped and other_idx stays stopped.
        clip_state = harness.last_state.get(f"/state/clip/{other_idx}/0", {})
        assert clip_state.get("is_playing") == 0, (
            "Non-group track restarted — scene was relaunched instead of group stopping"
        )

    def test_clip_stop_via_midi(self, harness):
        """Pressing a playing clip pad again should stop it (non-group track)."""
        harness.send_command("/clip/launch", ("i", "0"), ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("track") == 0 and m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )

        harness.press(PAD[0][0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("track") == 0 and m.get("scene") == 0 and m.get("is_playing") == 0,
            timeout=3.0,
        )
        assert msg.get("is_playing") == 0

    def test_select_pad_no_launch(self, harness):
        """Holding SELECT then pressing a pad should select the clip, not launch it."""
        harness.drain(timeout=0.1)
        with harness.holding(SELECT):
            harness.press(PAD[0][0])

        # Negative test: confirm clip did NOT start playing.
        # Use wait_for with a short timeout — TimeoutError means success (no launch).
        try:
            harness.wait_for(
                "/state/clip",
                predicate=lambda m: (
                    m.get("track") == 0 and m.get("scene") == 0
                    and m.get("is_playing") == 1
                ),
                timeout=1.0,
            )
        except TimeoutError:
            pass  # Expected: clip should NOT start playing
        else:
            pytest.fail("Clip started playing — SELECT+pad should select, not launch")

    def test_clear_pad_deletes_clip(self, harness, jam):
        """CLEAR+pad deletes a clip.

        Creates a temporary clip in an empty slot via /clip/create, deletes
        it with CLEAR+pad, and verifies has_content goes to 0.

        Skips if /clip/create is unavailable — we never fall back to
        deleting real project clips, as that risks permanent data loss if
        undo fails or the test aborts.
        """
        track, scene = 0, 7  # empty slot in test project

        # Create a temporary clip — skip if harness doesn't support it
        harness.send_command("/clip/create", ("i", str(track)), ("i", str(scene)))
        try:
            harness.wait_for(
                "/state/clip",
                predicate=lambda m: (
                    m.get("track") == track and m.get("scene") == scene
                    and m.get("has_content") == 1
                ),
                timeout=3.0,
            )
        except TimeoutError:
            pytest.skip("/clip/create not available — cannot safely test clip deletion")

        with harness.holding(CLEAR):
            jam.assert_mode_active("CLEAR")
            harness.press(PAD[scene][track])

        harness.wait_for(
            "/state/clip",
            predicate=lambda m: (
                m.get("track") == track and m.get("scene") == scene
                and m.get("has_content") == 0
            ),
            timeout=3.0,
        )

    def test_long_press_selects_without_launch(self, harness):
        """A long press on a clip pad should select without launching."""
        harness.drain(timeout=0.1)
        harness.press(PAD[0][0], hold_ms=1500)

        # Negative test: confirm clip did NOT start playing.
        try:
            harness.wait_for(
                "/state/clip",
                predicate=lambda m: (
                    m.get("track") == 0 and m.get("scene") == 0
                    and m.get("is_playing") == 1
                ),
                timeout=1.0,
            )
        except TimeoutError:
            pass  # Expected: clip should NOT start playing
        else:
            pytest.fail("Clip started playing — long press should select, not launch")

    def test_scene_launch(self, harness):
        """Launch scene 0 — all clips in scene should start playing."""
        harness.send_command("/scene/launch", ("i", "0"))

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1


class TestClipDuplicate:

    @pytest.mark.skip(reason="copySlotsOrScenes API doesn't update hasContent — Bitwig API limitation")
    def test_duplicate_clip_to_empty_slot(self, harness, jam):
        """Hold DUPLICATE, press source pad, press dest pad — copies clip.

        Uses track 0, scene 0 as source (known to have content) and
        track 0, scene 7 as destination (empty in test project).
        Cleans up by deleting the duplicated clip with CLEAR+pad.
        """
        source_state = harness.last_state.get("/state/clip/0/0", {})
        assert source_state.get("has_content") == 1, "Source clip must have content"

        with harness.holding(DUPLICATE):
            jam.assert_mode_active("DUPLICATE")
            harness.press(PAD[0][0])  # source: row 0, col 0
            time.sleep(0.1)
            harness.press(PAD[7][0])  # destination: row 7, col 0

        harness.wait_for(
            "/state/clip",
            predicate=lambda m: (
                m.get("track") == 0 and m.get("scene") == 7
                and m.get("has_content") == 1
            ),
            timeout=5.0,
        )

        # Clean up: delete the duplicated clip
        with harness.holding(CLEAR):
            jam.assert_mode_active("CLEAR")
            harness.press(PAD[7][0])

        harness.wait_for(
            "/state/clip",
            predicate=lambda m: (
                m.get("track") == 0 and m.get("scene") == 7
                and m.get("has_content") == 0
            ),
            timeout=3.0,
        )

    def test_duplicate_mode_is_gate(self, harness, jam):
        """DUPLICATE mode is a gate — active only while held."""
        with harness.holding(DUPLICATE):
            jam.assert_mode_active("DUPLICATE")
        jam.assert_mode_inactive("DUPLICATE")
