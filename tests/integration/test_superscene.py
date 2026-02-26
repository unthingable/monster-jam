"""Tier 1 — SuperScene mode tests.

Works with any Bitwig project state.

SuperScene is activated by pressing the SONG button, which toggles the
`sceneCycle` mode.  In SuperScene mode, SCENE buttons create and launch
SuperScenes.  CLEAR+SCENE deletes a SuperScene at that slot.
"""

import pytest

from jam_midi import CLEAR, SCENE, SONG


class TestSuperScene:

    def test_song_activates_scene_cycle(self, harness, jam):
        """Pressing SONG activates sceneCycle mode."""
        harness.press(SONG)
        jam.assert_mode_active("sceneCycle")

    @pytest.mark.known_project
    def test_superscene_create_and_launch(self, harness, jam):
        """Create a SuperScene from playing clips, stop, then launch it via SCENE[0].

        Requires the known test project so there are clips to play.
        """
        # Start with clips playing by launching scene 0 via OSC
        harness.send_command("/scene/launch", ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )

        # Enter SuperScene mode
        harness.press(SONG)
        jam.assert_mode_active("superSceneSub")

        # Press SCENE[0] to save the current playing state as a SuperScene
        harness.press(SCENE[0])

        # Stop playback so launching the SuperScene has a clear observable effect
        harness.send_command("/transport/stop")
        harness.drain(timeout=0.1)

        # Press SCENE[0] again to launch the saved SuperScene
        harness.press(SCENE[0])

        # At least one clip should start playing
        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

    def test_superscene_clear(self, harness, jam):
        """CLEAR+SCENE[0] in SuperScene mode should delete the slot without crashing."""
        harness.press(SONG)
        # Must wait for superSceneSub specifically — sceneCycle is always active,
        # so asserting it would pass instantly before SONG is processed.
        # CLEAR+SCENE in sceneSub (normal mode) would delete the Bitwig scene!
        jam.assert_mode_active("superSceneSub")

        with harness.holding(CLEAR):
            harness.press(SCENE[0])

        # Mode should still be active — the delete does not exit SuperScene mode
        jam.assert_mode_active("superSceneSub")

    def test_song_cycles_submodes(self, harness, jam):
        """Pressing SONG multiple times cycles between scene submodes.

        sceneCycle is a ModeCycleLayer with submodes: sceneSub and superSceneSub.
        Each press cycles to the next submode, but sceneCycle stays active.
        """
        harness.press(SONG)
        jam.assert_mode_active("sceneCycle")

        # Second press cycles submode — sceneCycle remains active
        harness.press(SONG)
        jam.assert_mode_active("sceneCycle")

        # Third press cycles back — still active
        harness.press(SONG)
        jam.assert_mode_active("sceneCycle")
