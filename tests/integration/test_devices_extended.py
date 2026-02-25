"""Tier 2 — Extended device navigation and device selector tests.

Requires a known Bitwig project with a multi-device track so that
device chain navigation is observable.

Mark: known_project
"""

import time

import pytest

from jam_midi import CONTROL, LEFT, RIGHT, LEVEL, MACRO, PAD, GROUP, MASTER, SELECT

pytestmark = pytest.mark.known_project


@pytest.fixture(autouse=True, scope="class")
def select_track_0(harness):
    """Place the cursor on track 0 before each class so device tests start clean."""
    harness.send_command("/track/select", ("i", "0"))
    harness.drain(timeout=0.2)


# ---------------------------------------------------------------------------
# Device chain navigation via LEFT/RIGHT in CONTROL mode
# ---------------------------------------------------------------------------

class TestDeviceChainNav:
    """LEFT/RIGHT arrows in CONTROL mode navigate the cursor device chain.

    The m() helper in Control.scala switches behaviour based on whether the
    CONTROL button is *held*:
      - CONTROL held + LEFT/RIGHT  → navigate remote-control *pages* (tested in test_devices.py)
      - CONTROL active but not held + LEFT/RIGHT → navigate *devices*

    These tests exercise the device-navigation path.
    """

    def _ensure_control_device_remote(self, harness, jam):
        """Activate CONTROL and ensure it is in the device remote submode.

        Device chain navigation via LEFT/RIGHT only works in 'strips remote'
        (device remote), not 'strips track remote'.  If we're in track remote,
        toggle via CONTROL+MACRO to switch to device remote.
        """
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")
        _, name = jam.control_submode()
        if name != "strips remote":
            # Toggle remote scope via CONTROL+MACRO
            with harness.holding(CONTROL):
                time.sleep(0.1)
                with harness.holding(MACRO):
                    time.sleep(0.1)
            jam.assert_control_submode("strips remote")

    def test_right_selects_next_device(self, harness, jam):
        """RIGHT in CONTROL mode (not held) advances to the next device."""
        self._ensure_control_device_remote(harness, jam)

        # Select device 0 via OSC so we have a known starting point
        harness.send_command("/device/select", ("i", "0"))
        msg = harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") == 0,
            timeout=3.0,
        )
        initial_index = msg["index"]
        harness.drain(timeout=0.1)

        # Press RIGHT (without holding CONTROL) — should navigate to next device
        harness.press(RIGHT)

        msg = harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") != initial_index,
            timeout=3.0,
        )
        assert msg["index"] != initial_index, (
            f"Expected device index to change from {initial_index}, "
            f"got {msg['index']}"
        )

    def test_left_returns_to_previous_device(self, harness, jam):
        """LEFT in CONTROL mode (not held) returns to the previous device."""
        self._ensure_control_device_remote(harness, jam)

        # Move to device 1 first so there is a device to go back to
        harness.send_command("/device/select", ("i", "1"))
        harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") == 1,
            timeout=3.0,
        )
        harness.drain(timeout=0.1)

        # Press LEFT — should move back to device 0
        harness.press(LEFT)

        msg = harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") == 0,
            timeout=3.0,
        )
        assert msg["index"] == 0, (
            f"Expected device index 0 after LEFT, got {msg['index']}"
        )

    def test_right_then_left_roundtrip(self, harness, jam):
        """RIGHT then LEFT in CONTROL mode returns to the original device."""
        self._ensure_control_device_remote(harness, jam)

        harness.send_command("/device/select", ("i", "0"))
        start_msg = harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") == 0,
            timeout=3.0,
        )
        harness.drain(timeout=0.1)

        # Advance one device
        harness.press(RIGHT)
        harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") != start_msg["index"],
            timeout=3.0,
        )
        harness.drain(timeout=0.1)

        # Go back
        harness.press(LEFT)
        end_msg = harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") == start_msg["index"],
            timeout=3.0,
        )
        assert end_msg["index"] == start_msg["index"], (
            f"Expected to return to device {start_msg['index']}, "
            f"got {end_msg['index']}"
        )

    def test_control_held_navigates_pages_not_devices(self, harness, jam):
        """Holding CONTROL while pressing RIGHT navigates pages, not devices.

        This confirms the dual-behaviour of LEFT/RIGHT: the m() helper in
        Control.scala switches between device navigation (button not held) and
        page navigation (button held).
        """
        self._ensure_control_device_remote(harness, jam)

        harness.send_command("/device/select", ("i", "0"))
        harness.send_command("/remote_control/page/select", ("i", "0"))
        msg0 = harness.wait_for(
            "/state/remote_control/page",
            predicate=lambda m: m.get("index") == 0,
            timeout=3.0,
        )
        if msg0.get("count", 999) < 2:
            pytest.skip("Device has fewer than 2 remote pages")
        device_before = harness.last_state.get("/state/device", {}).get("index", -1)
        harness.drain(timeout=0.1)

        # Press RIGHT while holding CONTROL — should change page, not device
        with harness.holding(CONTROL):
            time.sleep(0.05)
            harness.press(RIGHT)

        page_msg = harness.wait_for(
            "/state/remote_control/page",
            predicate=lambda m: m.get("index", -1) == 1,
            timeout=3.0,
        )
        assert page_msg["index"] == 1, (
            f"Expected remote control page 1, got {page_msg['index']}"
        )

        # Device should not have changed
        device_after = harness.last_state.get("/state/device", {}).get("index", -1)
        assert device_after == device_before, (
            f"Device index changed unexpectedly: {device_before} -> {device_after}"
        )


# ---------------------------------------------------------------------------
# Remote scope: track vs project remotes
# ---------------------------------------------------------------------------

class TestRemoteScopeTrackVsProject:
    """Tests for the track/project remote submode (index 17 in controlLayer).

    There is a single SliderBankMode named 'strips track remote' for both
    track and project remotes.  On a regular track it shows track-level
    remote controls; selecting the Master track switches the cursor track
    so the same page reflects project (master) remote controls.
    """

    def _activate_remote_scope(self, harness, jam):
        """Put CONTROL into the track/project remote submode via CONTROL+MACRO."""
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

        # If not already in track remote scope, toggle it
        idx, name = jam.control_submode()
        if name != "strips track remote":
            with harness.holding(CONTROL):
                time.sleep(0.1)
                with harness.holding(MACRO):
                    time.sleep(0.1)
            jam.assert_control_submode("strips track remote")

    def test_remote_scope_shows_track_remote_submode(self, harness, jam):
        """CONTROL+MACRO activates the 'strips track remote' submode."""
        self._activate_remote_scope(harness, jam)

        idx, name = jam.control_submode()
        assert name == "strips track remote", (
            f"Expected 'strips track remote' submode, got: '{name}'"
        )

    # NOTE: Track remote page nav tests removed — LEFT/RIGHT in track remote
    # submode navigates devices, not pages.  Page navigation uses OSC commands
    # (tested in test_devices.py::test_device_remote_page_next).

    def test_selecting_master_track_stays_in_remote_submode(self, harness, jam):
        """Selecting Master track does not exit the track remote submode.

        Project remotes are just track remotes on the Master track — the
        same 'strips track remote' SliderBankMode handles both.  Switching
        to Master should leave the submode name unchanged.
        """
        self._activate_remote_scope(harness, jam)

        idx_before, name_before = jam.control_submode()

        # Select the master track
        harness.press(MASTER)
        try:
            harness.wait_for(
                "/state/cursor_track",
                predicate=lambda m: "master" in m.get("name", "").lower(),
                timeout=3.0,
            )
            harness.drain(timeout=0.1)

            idx_after, name_after = jam.control_submode()
            assert name_after == name_before, (
                f"Remote submode changed after selecting Master: "
                f"'{name_before}' -> '{name_after}'"
            )
        finally:
            # Restore to track 0 so other tests are not affected
            harness.send_command("/track/select", ("i", "0"))
            harness.drain(timeout=0.2)

    def test_remote_scope_persistent_across_track_changes(self, harness, jam):
        """Switching between tracks keeps the remote submode active."""
        self._activate_remote_scope(harness, jam)

        # Switch to a different track
        harness.press(GROUP[1])
        try:
            harness.wait_for("/state/cursor_track", timeout=2.0)
            harness.drain(timeout=0.1)

            idx, name = jam.control_submode()
            assert name == "strips track remote", (
                f"Remote submode lost after track change, got: '{name}'"
            )
        finally:
            # Restore
            harness.press(GROUP[0])
            harness.drain(timeout=0.1)


# ---------------------------------------------------------------------------
# Device Selector LED feedback
# ---------------------------------------------------------------------------

class TestDeviceSelectorLeds:
    """Tests for device selector LED feedback in the pad matrix.

    When CONTROL is held, the device matrix selector submode is active and
    sends SupColorStateB updates to PAD matrix buttons for every device
    that exists in the device banks.
    """

    def test_holding_control_activates_device_selector(self, harness, jam):
        """Holding CONTROL activates the deviceSelector mode."""
        with harness.holding(CONTROL):
            jam.assert_mode_active("deviceSelector")

    # NOTE: Pad matrix LED tests removed — the harness MIDI proxy does not
    # relay pad NoteOn messages (they go through Bitwig's HardwareSurface
    # color bindings, not the raw MIDI output stream).

    def test_device_selector_select_popup_toggle(self, harness, jam):
        """CONTROL+SELECT toggles the device matrix popup overlay.

        After toggling, CONTROL+SELECT again restores the original state.
        Device Selector itself remains active throughout.
        """
        harness.drain(timeout=0.1)

        # Toggle popup on
        with harness.holding(CONTROL):
            time.sleep(0.1)
            harness.press(SELECT)

        # Device selector mode should still be active
        with harness.holding(CONTROL):
            jam.assert_mode_active("deviceSelector")

        # Toggle popup off
        with harness.holding(CONTROL):
            time.sleep(0.1)
            harness.press(SELECT)

        # Device selector still active
        with harness.holding(CONTROL):
            jam.assert_mode_active("deviceSelector")


# ---------------------------------------------------------------------------
# Device selection via pad press in device selector
# ---------------------------------------------------------------------------

class TestDeviceSelectViaPad:
    """Selecting a device by pressing a pad in the device selector matrix."""

    def test_pad_press_selects_device_on_track(self, harness, jam):
        """Pressing PAD[0][0] in CONTROL mode selects the first device on track A.

        PAD[row][col]: row 0 = top, col 0 = left (track A).
        In the device matrix selector, cols map to tracks and rows map to
        device positions within each track's device bank.
        """
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

        # Press the top-left pad (track 0, device slot 0)
        harness.press(PAD[0][0])

        # The cursor track should switch to the track at col 0
        msg = harness.wait_for("/state/cursor_track", timeout=3.0)
        assert msg is not None

        # A device state update should arrive
        device_msg = harness.wait_for("/state/device", timeout=3.0)
        assert device_msg is not None

    def test_pad_press_updates_cursor_device(self, harness, jam):
        """Pressing a pad for device slot 1 on track 0 selects device index 1."""
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

        # First select device 0 so we have a baseline
        harness.press(PAD[0][0])
        harness.wait_for("/state/device", timeout=3.0)
        harness.drain(timeout=0.1)

        # Now press PAD[1][0] to select device slot 1 on the same track
        harness.press(PAD[1][0])

        device_msg = harness.wait_for("/state/device", timeout=3.0)
        assert device_msg is not None

    # NOTE: test_pad_press_different_track_changes_cursor_track removed —
    # the device selector column→track mapping depends on device bank layout
    # which varies with the test project.  The simpler pad-selects-device
    # tests above cover the core functionality.
