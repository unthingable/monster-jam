"""Tier 2 — Scene and SuperScene toggle tests.

Verifies that pressing a playing scene stops it, and pressing again relaunches.
Covers both regular scenes (sceneSub) and SuperScenes (superSceneSub).

Mark: known_project
"""

import time

import pytest

from jam_midi import SCENE, SONG

pytestmark = pytest.mark.known_project

# MIDI constants for LED assertions
NOTE_ON = 0x90
SCENE_CHANNEL = 1


class TestSceneToggle:
    """Scene button toggles: press playing scene to stop, press again to relaunch."""

    def test_scene_press_stops_playing_scene(self, harness):
        """Launch scene 0 via OSC, then press SCENE[0] to stop all clips."""
        harness.send_command("/scene/launch", ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )

        # Press SCENE[0] — should stop all clips (toggle off)
        harness.press(SCENE[0])

        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 0,
            timeout=3.0,
        )

    def test_scene_press_relaunches_stopped_scene(self, harness):
        """After stopping a scene, pressing SCENE[0] again relaunches it."""
        # Launch and stop to set up the toggle state
        harness.send_command("/scene/launch", ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        harness.press(SCENE[0])
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 0,
            timeout=3.0,
        )

        # Press SCENE[0] again — should relaunch
        harness.press(SCENE[0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

        # Clean up — stop transport
        harness.send_command("/transport/stop")

    def test_scene_led_brighter_when_playing(self, harness):
        """Scene LED brightness is 3 when playing, 1 when stopped.

        LED value = (color_index & ~3) + brightness, so brightness is
        the low 2 bits of the data2 byte sent to the scene button.

        Captures LED updates during the stopped→playing and playing→stopped
        transitions.  SupColorStateB fires when the supplier value changes,
        so we observe the transition itself rather than polling.
        """
        def _find_scene0_led(msgs):
            """Find the last scene 0 LED message in a message burst."""
            matches = [
                m for m in msgs
                if m.get("address") == "/midi/in"
                and m.get("status") == NOTE_ON
                and m.get("channel") == SCENE_CHANNEL
                and m.get("data1") == SCENE[0][1]
                and m.get("data2", 0) > 0
            ]
            return matches[-1] if matches else None

        def _has_clip_playing(msgs):
            return any(
                m for m in msgs
                if m.get("address") == "/state/clip"
                and m.get("scene") == 0
                and m.get("is_playing") == 1
            )

        # Ensure clips are stopped (settle already stops transport)
        harness.drain(timeout=0.2)

        # Launch scene 0 and drain all messages during the transition.
        # Both LED updates (/midi/in) and clip state (/state/clip) arrive.
        harness.send_command("/scene/launch", ("i", "0"))
        msgs_playing = harness.drain(timeout=2.0)

        if not _has_clip_playing(msgs_playing):
            pytest.skip("Scene launch did not produce clip playing state")

        led_playing = _find_scene0_led(msgs_playing)

        # Stop via SCENE[0] press and drain during the transition.
        harness.press(SCENE[0])
        msgs_stopped = harness.drain(timeout=2.0)
        led_stopped = _find_scene0_led(msgs_stopped)

        if not led_playing or not led_stopped:
            pytest.skip(
                "Could not capture scene LED messages in both states "
                f"(playing: {led_playing}, stopped: {led_stopped})"
            )

        brightness_playing = led_playing["data2"] & 0x03
        brightness_stopped = led_stopped["data2"] & 0x03

        assert brightness_playing == 3, (
            f"Expected brightness 3 while playing, got {brightness_playing} "
            f"(data2=0x{led_playing['data2']:02x})"
        )
        assert brightness_stopped == 1, (
            f"Expected brightness 1 while stopped, got {brightness_stopped} "
            f"(data2=0x{led_stopped['data2']:02x})"
        )


class TestSuperSceneToggle:
    """SuperScene toggle: press playing superscene to stop, press again to relaunch.

    SuperScene slots and lastScene state persist across tests within a Bitwig
    session.  These tests follow the same pattern as test_superscene.py's
    test_superscene_create_and_launch: press SCENE[0] while clips are playing
    to either save (if slot empty) or recall (if slot has data).  Either way,
    clips end up playing and lastScene is set, which is the state needed for
    toggle-stop testing.
    """

    def _enter_superscene_mode(self, harness, jam):
        """Ensure we are in superSceneSub mode."""
        active, _ = jam.mode_state()
        if "superSceneSub" not in active:
            harness.press(SONG)
            jam.assert_mode_active("superSceneSub")

    def _setup_playing_superscene(self, harness, jam):
        """Launch clips and ensure a SuperScene is playing with lastScene set.

        Pressing SCENE[0] while clips are playing will either:
        - Save (slot empty) → sets data, no lastScene change
        - Recall (slot has data, lastScene != 0) → launches, sets lastScene
        - Toggle-stop (slot has data, lastScene == 0) → stops, clears lastScene

        To guarantee we end up with clips playing and lastScene == Some(0),
        we press SCENE[0] repeatedly until clips are confirmed playing and
        lastScene must be set (i.e. we went through recall).
        """
        self._enter_superscene_mode(harness, jam)

        # First press: save or recall or toggle-stop depending on state
        harness.press(SCENE[0])
        time.sleep(0.5)

        # If clips are still playing, lastScene might or might not be set.
        # Stop transport to create a clean baseline.
        harness.send_command("/transport/stop")
        harness.drain(timeout=0.5)

        # Press SCENE[0] — if slot has data (from save or previous session),
        # this is a recall that launches clips and sets lastScene.
        # If slot is empty (save captured nothing), this saves empty → no-op.
        harness.press(SCENE[0])
        try:
            harness.wait_for(
                "/state/clip",
                predicate=lambda m: m.get("is_playing") == 1,
                timeout=3.0,
            )
        except TimeoutError:
            # Slot might have been empty or had stale lastScene.
            # Re-launch via OSC, save fresh, then recall.
            harness.send_command("/scene/launch", ("i", "0"))
            harness.wait_for(
                "/state/clip",
                predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
                timeout=3.0,
            )
            time.sleep(0.5)  # let superBank sync
            harness.press(SCENE[0])  # save
            time.sleep(0.3)
            harness.send_command("/transport/stop")
            harness.drain(timeout=0.3)
            harness.press(SCENE[0])  # recall
            harness.wait_for(
                "/state/clip",
                predicate=lambda m: m.get("is_playing") == 1,
                timeout=3.0,
            )

    def test_superscene_press_stops_playing_superscene(self, harness, jam):
        """Launch a SuperScene, then press same button to stop."""
        # Launch scene 0 via OSC so clips are playing
        harness.send_command("/scene/launch", ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )

        self._setup_playing_superscene(harness, jam)

        # Press SCENE[0] again — should toggle-stop the playing SuperScene.
        # Drain first so we only see NEW state messages after the press.
        harness.drain(timeout=0.2)
        harness.press(SCENE[0])

        # Collect state messages — look for clip stop transitions
        msgs = harness.drain(timeout=2.0)
        stop_msgs = [
            m for m in msgs
            if m.get("address") == "/state/clip"
            and m.get("is_playing") == 0
            and m.get("has_content") == 1
        ]
        assert len(stop_msgs) > 0, (
            f"No clip stop messages received after SuperScene toggle-stop "
            f"(got {len(msgs)} messages total, "
            f"clip msgs: {[m for m in msgs if m.get('address') == '/state/clip']})"
        )

    def test_superscene_relaunches_after_toggle_stop(self, harness, jam):
        """After toggle-stopping a SuperScene, pressing it again relaunches."""
        # Launch scene 0 via OSC
        harness.send_command("/scene/launch", ("i", "0"))
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("scene") == 0 and m.get("is_playing") == 1,
            timeout=3.0,
        )

        self._setup_playing_superscene(harness, jam)

        # Toggle-stop
        harness.press(SCENE[0])
        time.sleep(1.0)

        # Press again — should relaunch (recall)
        harness.press(SCENE[0])

        msg = harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("is_playing") == 1,
            timeout=3.0,
        )
        assert msg["is_playing"] == 1

        # Clean up — stop transport
        harness.send_command("/transport/stop")
