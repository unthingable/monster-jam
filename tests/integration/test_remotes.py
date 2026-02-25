"""Tier 1 — Track & project remote control tests.

Tests the CONTROL+MACRO toggle between device and track/project remote
scope, mode stickiness, and that MACRO-alone behavior is preserved.
Works with any Bitwig project state.
"""

import time

import pytest

from jam_midi import CONTROL, MACRO, LEFT, RIGHT, LEVEL


def _ensure_device_remote(harness, jam):
    """Ensure CONTROL is active at device remote (submode 0).

    Deactivates any strip mode, re-activates CONTROL, then toggles
    back to device remote if stickiness left us in remote scope.
    """
    harness.press(LEVEL)
    jam.assert_mode_active("LEVEL")
    harness.press(CONTROL)
    jam.assert_mode_active("CONTROL")

    # Check if stickiness left us in remote scope — if so, toggle back
    idx, name = jam.control_submode()
    if name != "strips remote":
        _toggle_remote(harness)
        jam.assert_control_submode("strips remote")


def _toggle_remote(harness):
    """Toggle between device and track/project remote scope.

    Must be called when CONTROL is already active. Holds CONTROL,
    presses MACRO (which triggers the toggle in MacroL.onActivate),
    then releases both.
    """
    with harness.holding(CONTROL):
        time.sleep(0.1)
        with harness.holding(MACRO):
            time.sleep(0.1)
        time.sleep(0.1)


class TestRemoteScope:

    def test_control_macro_activates_remote_scope(self, harness, jam):
        """CONTROL+MACRO toggles from device remotes to track/project remotes."""
        _ensure_device_remote(harness, jam)
        _toggle_remote(harness)

        idx, name = jam.control_submode()
        assert "remote" in name and name != "strips remote", (
            f"Expected track/project remote submode, got: '{name}' (idx={idx})"
        )

    def test_control_macro_toggles_back(self, harness, jam):
        """CONTROL+MACRO twice returns to device remotes."""
        _ensure_device_remote(harness, jam)

        # First toggle: device → remote scope
        _toggle_remote(harness)
        idx, name = jam.control_submode()
        assert "remote" in name and name != "strips remote", (
            f"Expected remote mode after first toggle, got: '{name}'"
        )

        # Second toggle: remote scope → device
        _toggle_remote(harness)
        jam.assert_control_submode("strips remote")

    def test_remote_scope_stickiness(self, harness, jam):
        """Remote scope persists across CONTROL deactivation/reactivation."""
        _ensure_device_remote(harness, jam)
        _toggle_remote(harness)

        idx, name = jam.control_submode()
        assert "remote" in name and name != "strips remote", (
            f"Expected remote mode, got: '{name}'"
        )

        # Deactivate CONTROL by switching to another strip mode
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")
        jam.assert_mode_inactive("CONTROL")

        # Reactivate CONTROL — should still be in remote scope
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

        idx, name = jam.control_submode()
        assert "remote" in name and name != "strips remote", (
            f"Remote scope not sticky after reactivation, got: '{name}'"
        )

    def test_macro_alone_unchanged(self, harness, jam):
        """MACRO without CONTROL still activates user banks (not remotes)."""
        # Ensure CONTROL is not active
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        with harness.holding(MACRO):
            jam.assert_mode_active("MACRO")

            idx, name = jam.control_submode()
            assert "user bank" in name, (
                f"MACRO alone should select user bank, got: '{name}' (idx={idx})"
            )

    def test_page_nav_in_remote_scope(self, harness, jam):
        """Left/Right navigate remote control pages when in remote scope."""
        _ensure_device_remote(harness, jam)
        _toggle_remote(harness)

        idx, name = jam.control_submode()
        assert "remote" in name and name != "strips remote", (
            f"Expected remote mode, got: '{name}'"
        )

        # Press RIGHT — should stay in remote scope
        harness.press(RIGHT)
        idx2, name2 = jam.control_submode()
        assert name2 == name, (
            f"Right arrow should not change submode, was '{name}' now '{name2}'"
        )

        # Press LEFT — should stay in remote scope
        harness.press(LEFT)
        idx3, name3 = jam.control_submode()
        assert name3 == name, (
            f"Left arrow should not change submode, was '{name}' now '{name3}'"
        )
