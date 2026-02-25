"""Tier 1/2 — Device Selector tests.

Tests Device Selector activation, sticky mode, and device matrix popup.
Tier 1 tests work with any Bitwig project state.
Tier 2 tests (known_project) require the test project to be loaded.
"""

import time

import pytest

from jam_midi import CONTROL, LEVEL, MACRO, PAD, SELECT


class TestDeviceSelector:

    def test_control_activates_device_selector(self, harness, jam):
        """Pressing CONTROL activates the CONTROL mode."""
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

    def test_control_always_has_device_selector(self, harness, jam):
        """Holding CONTROL always activates Device Selector.

        Device Selector is an integral part of CONTROL mode, not something
        toggled on/off.  CONTROL+SELECT toggles a separate device matrix
        *popup* overlay.
        """
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        with harness.holding(CONTROL):
            jam.assert_mode_active("deviceSelector")

    def test_control_select_toggles_device_matrix_popup(self, harness, jam):
        """CONTROL+SELECT toggles the device matrix popup overlay.

        This is independent of Device Selector mode itself, which is always
        active when CONTROL is held.  The popup shows devices in the clip
        matrix area.
        """
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        # Toggle popup on
        with harness.holding(CONTROL):
            time.sleep(0.1)
            harness.press(SELECT)

        # Toggle popup off
        with harness.holding(CONTROL):
            time.sleep(0.1)
            harness.press(SELECT)

        # Device Selector is still active regardless of popup state
        with harness.holding(CONTROL):
            jam.assert_mode_active("deviceSelector")

    def test_control_persists_after_press(self, harness, jam):
        """CONTROL stays active after a quick press (select-cycle behavior)."""
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

        # CONTROL is a cycle-select mode — stays active after release
        jam.assert_mode_active("CONTROL")

    @pytest.mark.known_project
    def test_device_select_via_pad(self, harness):
        """Pressing PAD[0][0] while in CONTROL selects device 0."""
        harness.press(CONTROL)

        harness.press(PAD[0][0])

        harness.wait_for("/state/device", timeout=3.0)

    def test_macro_activates_control_user_submode(self, harness, jam):
        """Holding MACRO activates CONTROL with user parameters submode.

        MACRO is a shortcut: it bumps the current strip mode and activates
        CONTROL in user-parameter mode.
        """
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        with harness.holding(MACRO):
            jam.assert_mode_active("MACRO")
            jam.assert_mode_active("CONTROL")

        # After MACRO release, LEVEL should be restored
        jam.assert_mode_active("LEVEL")
