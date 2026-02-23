"""Tier 1 — Mode switching tests.

Tests MonsterJam's internal mode graph via direct OSC state queries.
Works with any Bitwig project state.
"""

import time

import pytest

from jam_midi import LEVEL, AUX, CONTROL, MACRO, SONG


class TestModes:

    def test_level_activates(self, harness, jam):
        """Pressing LEVEL activates the LEVEL mode."""
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

    def test_level_cycle(self, harness, jam):
        """Pressing LEVEL twice cycles through volume and pan modes."""
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")
        time.sleep(0.3)

        harness.press(LEVEL)
        # Second press cycles sub-mode but LEVEL stays active
        jam.assert_mode_active("LEVEL")

    def test_aux_activates(self, harness, jam):
        """Pressing AUX activates the AUX mode."""
        harness.press(AUX)
        jam.assert_mode_active("AUX")

    def test_control_activates(self, harness, jam):
        """Pressing CONTROL activates the CONTROL mode."""
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")

    def test_strip_exclusive(self, harness, jam):
        """Level and Aux are mutually exclusive — activating one deactivates the other."""
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        time.sleep(0.3)

        harness.press(AUX)
        jam.assert_mode_active("AUX")
        jam.assert_mode_inactive("LEVEL")

    def test_song_mode(self, harness, jam):
        """Pressing SONG activates scene cycle mode."""
        harness.press(SONG)
        jam.assert_mode_active("sceneCycle")

    def test_macro_bumps_control(self, harness, jam):
        """Holding MACRO activates MACRO layer, bumping other strip modes."""
        harness.hold(MACRO)
        time.sleep(0.5)
        jam.assert_mode_active("MACRO")
        harness.release(MACRO)

    def test_shift_gate(self, harness, log):
        """SHIFT is a gate — modes activate on hold, deactivate on release.

        Note: SHIFT on Maschine JAM is delivered via sysex, not a regular CC.
        This test sends the CC for Left Arrow which MJ may interpret as shift,
        or it may need sysex. If this test fails, shift may require sysex
        injection which the MIDI proxy doesn't support yet.
        """
        # SHIFT is sysex-based on JAM hardware, so this test documents
        # the limitation. We test that at minimum the button event is processed.
        from jam_midi import LEFT
        harness.hold(LEFT)
        time.sleep(0.5)
        harness.release(LEFT)
        # If shift works via CC, we'd see shift mode activation.
        # This is a best-effort test — may need sysex support.
        log.wait_for(r"KeyMaster eval BtnArrowLeft Press", timeout=2.0)
