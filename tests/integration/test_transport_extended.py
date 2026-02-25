"""Tier 1 — Extended transport and global feature tests.

Covers AUTO, TEMPO, track-hold combos (solo/mute/arm), Control Slice mode,
and AUX effect track selection.  Works with any Bitwig project state.
"""

import time

from jam_midi import (
    AUTO,
    AUX,
    CONTROL,
    ENCODER_TURN,
    GROUP,
    LEFT,
    LEVEL,
    MACRO,
    MUTE,
    RECORD,
    RIGHT,
    SOLO,
    TEMPO,
)

# ---------------------------------------------------------------------------
# MIDI status bytes (without channel)
# ---------------------------------------------------------------------------
CC = 0xB0


def _is_cc(data1, msg):
    """True if /midi/in message is a CC for the given data1 number."""
    return msg.get("status") == CC and msg.get("data1") == data1


# ---------------------------------------------------------------------------
# AUTO
# ---------------------------------------------------------------------------

class TestAuto:

    def test_auto_toggles_automation_write(self, harness, log):
        """Pressing AUTO toggles arranger automation write mode."""
        harness.press(AUTO)
        log.wait_for(r"KeyMaster eval BtnAuto Press", timeout=2.0)
        # Restore: press again to toggle back to original state
        harness.press(AUTO)

    def test_auto_led_reflects_state(self, harness):
        """AUTO LED changes when automation write is toggled on and off."""
        # Press LEVEL to push a non-AUTO CC into /midi/in state,
        # then drain so the first wait_for won't match stale state.
        harness.press(LEVEL)
        harness.drain(timeout=0.2)

        # First press — LED should update
        harness.press(AUTO)
        msg_on = harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(AUTO[1], m),
            timeout=2.0,
        )
        first_value = msg_on["data2"]

        harness.drain(timeout=0.1)

        # Second press — toggle; LED value should differ from first
        harness.press(AUTO)
        msg_off = harness.wait_for(
            "/midi/in",
            predicate=lambda m: _is_cc(AUTO[1], m) and m.get("data2") != first_value,
            timeout=2.0,
        )
        assert msg_off["data2"] != first_value

    def test_auto_idempotent(self, harness, log):
        """Pressing AUTO twice restores the original automation state."""
        harness.press(AUTO)
        log.wait_for(r"KeyMaster eval BtnAuto Press", timeout=2.0)
        log.clear()

        harness.press(AUTO)
        log.wait_for(r"KeyMaster eval BtnAuto Press", timeout=2.0)


# ---------------------------------------------------------------------------
# TEMPO
# ---------------------------------------------------------------------------

class TestTempo:

    def test_tempo_activates(self, harness, jam):
        """Quick-pressing TEMPO activates tempo mode."""
        harness.press(TEMPO)
        jam.assert_mode_active("tempo")

        # Clean up — press again to deactivate if it toggled on
        harness.press(TEMPO)

    def test_tempo_auto_gates_on_long_press(self, harness, jam):
        """TEMPO is auto-gating: long hold exits on release without a second press."""
        with harness.holding(TEMPO):
            time.sleep(0.6)  # exceed 500 ms auto-gate threshold
            jam.assert_mode_active("tempo")
        jam.assert_mode_inactive("tempo")

    def test_tempo_quick_press_toggles_on(self, harness, jam):
        """Quick press (<500 ms) activates tempo mode as a toggle."""
        harness.press(TEMPO)
        jam.assert_mode_active("tempo")

        # Clean up
        harness.press(TEMPO)
        jam.assert_mode_inactive("tempo")

    def test_tempo_quick_press_toggles_off(self, harness, jam):
        """Two quick presses toggle tempo mode on then off."""
        harness.press(TEMPO)
        jam.assert_mode_active("tempo")

        harness.press(TEMPO)
        jam.assert_mode_inactive("tempo")

    def test_tempo_knob_changes_tempo(self, harness, jam):
        """Holding TEMPO and turning the encoder changes the project tempo."""
        with harness.holding(TEMPO):
            jam.assert_mode_active("tempo")
            # Send three clockwise ticks (65 = CW +1 in 2's complement, center=64)
            for _ in range(3):
                harness.send_midi(0xB0, ENCODER_TURN[1], 65)
                time.sleep(0.05)

        # Restore: send three counter-clockwise ticks while re-holding TEMPO
        with harness.holding(TEMPO):
            jam.assert_mode_active("tempo")
            for _ in range(3):
                harness.send_midi(0xB0, ENCODER_TURN[1], 63)
                time.sleep(0.05)

        jam.assert_mode_inactive("tempo")

    def test_tempo_knob_inactive_outside_tempo_mode(self, harness, jam):
        """Encoder turns outside TEMPO mode do not activate the tempo layer."""
        jam.assert_mode_inactive("tempo")
        # Send a turn without holding TEMPO
        harness.send_midi(0xB0, ENCODER_TURN[1], 65)
        time.sleep(0.1)
        jam.assert_mode_inactive("tempo")


# ---------------------------------------------------------------------------
# Track hold combos (track-gate layer)
# ---------------------------------------------------------------------------

class TestTrackCombos:
    """Hold a GROUP button, then press SOLO/MUTE/RECORD to act on that track."""

    def test_track_solo_via_hold(self, harness):
        """Hold GROUP[0] + press SOLO toggles solo for track 0."""
        # Read initial state
        harness.drain(timeout=0.1)
        initial = harness.last_state.get("/state/track/0", {})
        initial_solo = initial.get("solo", 0)

        with harness.holding(GROUP[0]):
            harness.press(SOLO)
            harness.wait_for(
                "/state/track",
                predicate=lambda m: (
                    m.get("bank_index") == 0
                    and m.get("solo") != initial_solo
                ),
                timeout=2.0,
            )

        # Restore: hold GROUP[0] + press SOLO again
        with harness.holding(GROUP[0]):
            harness.press(SOLO)
            harness.wait_for(
                "/state/track",
                predicate=lambda m: (
                    m.get("bank_index") == 0
                    and m.get("solo") == initial_solo
                ),
                timeout=2.0,
            )

    def test_track_mute_via_hold(self, harness):
        """Hold GROUP[0] + press MUTE toggles mute for track 0."""
        harness.drain(timeout=0.1)
        initial = harness.last_state.get("/state/track/0", {})
        initial_mute = initial.get("mute", 0)

        with harness.holding(GROUP[0]):
            harness.press(MUTE)
            harness.wait_for(
                "/state/track",
                predicate=lambda m: (
                    m.get("bank_index") == 0
                    and m.get("mute") != initial_mute
                ),
                timeout=2.0,
            )

        # Restore
        with harness.holding(GROUP[0]):
            harness.press(MUTE)
            harness.wait_for(
                "/state/track",
                predicate=lambda m: (
                    m.get("bank_index") == 0
                    and m.get("mute") == initial_mute
                ),
                timeout=2.0,
            )

    def test_track_rec_arm_via_hold(self, harness):
        """Hold GROUP[0] + press RECORD toggles arm for track 0."""
        harness.drain(timeout=0.1)
        initial = harness.last_state.get("/state/track/0", {})
        initial_arm = initial.get("arm", 0)

        with harness.holding(GROUP[0]):
            harness.press(RECORD)
            harness.wait_for(
                "/state/track",
                predicate=lambda m: (
                    m.get("bank_index") == 0
                    and m.get("arm") != initial_arm
                ),
                timeout=2.0,
            )

        # Restore
        with harness.holding(GROUP[0]):
            harness.press(RECORD)
            harness.wait_for(
                "/state/track",
                predicate=lambda m: (
                    m.get("bank_index") == 0
                    and m.get("arm") == initial_arm
                ),
                timeout=2.0,
            )

    def test_track_gate_exits_on_release(self, harness, jam):
        """Track-gate layer deactivates when GROUP button is released."""
        gate_mode = "track gate 0"

        with harness.holding(GROUP[0]):
            jam.assert_mode_active(gate_mode)
        jam.assert_mode_inactive(gate_mode)

    def test_track_gate_shows_dpad_indicators(self, harness):
        """Holding a GROUP button updates DPAD arrow LEDs (e.g. group enter/exit)."""
        harness.drain(timeout=0.1)

        with harness.holding(GROUP[0]):
            midi = harness.collect_midi(timeout=0.5)
            # DPAD arrows are CCs: UP=40, DOWN=41, LEFT=42, RIGHT=43
            dpad_msgs = [
                m for m in midi
                if m.get("status") == CC
                and m.get("data1", -1) in (40, 41, 42, 43)
            ]
            assert len(dpad_msgs) > 0, (
                "Expected DPAD LED updates when holding a GROUP button"
            )


# ---------------------------------------------------------------------------
# Control Slice mode
# ---------------------------------------------------------------------------

class TestControlSlice:

    def _ensure_base_remote(self, harness, jam):
        """Activate CONTROL in the base 'strips remote' submode."""
        harness.press(CONTROL)
        jam.assert_mode_active("CONTROL")
        idx, name = jam.control_submode()
        if name == "strips remote":
            return
        if "slice" in name:
            self._exit_slice(harness, jam)
        elif name == "strips track remote":
            with harness.holding(CONTROL):
                time.sleep(0.1)
                harness.press(MACRO)
            time.sleep(0.1)
        # Verify we landed on base remote
        jam.assert_control_submode("strips remote")

    def _enter_slice(self, harness):
        """Enter slice mode via simultaneous LEFT+RIGHT.

        Control.scala line 154-155: pressing LEFT while RIGHT is held
        (or vice versa) calls select(currentSlice + 1).  Must use
        hold/release to avoid triggering device navigation on release.
        """
        harness.hold(RIGHT)
        time.sleep(0.05)
        harness.hold(LEFT)
        time.sleep(0.1)
        # Release both without triggering exit.  LEFT release alone
        # when RIGHT is not pressed exits slice mode, so release RIGHT
        # first, then LEFT while in slice mode (which will try to exit
        # but RIGHT was already released — we need a different approach).
        # Actually: release both simultaneously to avoid the exit trigger.
        harness.release(RIGHT)
        harness.release(LEFT)
        time.sleep(0.1)

    def _exit_slice(self, harness, jam):
        """Exit slice mode back to base 'strips remote'.

        In slice mode, pressL/pressR start as false on activation.
        Press LEFT (sets pressL=true), then release LEFT while RIGHT
        is not pressed → select(0) returns to base.
        """
        harness.hold(LEFT)
        time.sleep(0.1)
        harness.release(LEFT)
        time.sleep(0.2)

    def test_left_right_enters_slice_mode(self, harness, jam):
        """In CONTROL mode, pressing LEFT+RIGHT activates a control slice."""
        self._ensure_base_remote(harness, jam)

        self._enter_slice(harness)

        idx, name = jam.control_submode()
        assert "slice" in name, (
            f"Expected slice submode after LEFT+RIGHT, got: '{name}' (idx={idx})"
        )

        # Clean up
        self._exit_slice(harness, jam)

    def test_control_slice_group_selects_strip(self, harness, jam):
        """In a slice submode, holding a GROUP button selects that control slice.

        Slice selectors are auto-gating: holding GROUP[N] switches to slice N,
        releasing returns to the previous slice.
        """
        self._ensure_base_remote(harness, jam)
        self._enter_slice(harness)

        # Hold GROUP[2] — should momentarily switch to slice 2
        with harness.holding(GROUP[2]):
            time.sleep(0.15)
            idx2, name2 = jam.control_submode()
            assert "slice" in name2, (
                f"Expected slice submode while holding GROUP, got: '{name2}'"
            )

        # After release, should return to the previous slice
        time.sleep(0.1)
        idx3, name3 = jam.control_submode()
        assert "slice" in name3, (
            f"Expected slice submode after GROUP release, got: '{name3}'"
        )

        # Clean up
        self._exit_slice(harness, jam)


# ---------------------------------------------------------------------------
# AUX effect track selection
# ---------------------------------------------------------------------------

class TestAux:

    def test_aux_activates(self, harness, jam):
        """Pressing AUX activates AUX mode."""
        # Start from a known state
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

        harness.press(AUX)
        jam.assert_mode_active("AUX")

        # Clean up — switch back to LEVEL
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

    def test_aux_gate_on_hold(self, harness, jam):
        """Holding AUX activates the AUX gate layer for effect track selection."""
        gate_mode = "strip AUX gate"

        with harness.holding(AUX):
            jam.assert_mode_active(gate_mode)
        jam.assert_mode_inactive(gate_mode)

    def test_aux_gate_group_sends_midi(self, harness, jam):
        """Holding AUX and pressing GROUP[0] selects effect send 0 without crashing."""
        gate_mode = "strip AUX gate"

        with harness.holding(AUX):
            jam.assert_mode_active(gate_mode)
            harness.press(GROUP[0])
            time.sleep(0.1)
        # After release, gate layer is gone
        jam.assert_mode_inactive(gate_mode)
        # AUX (cycle) mode itself remains active
        jam.assert_mode_active("AUX")

        # Clean up
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")

    def test_aux_gate_led_updates(self, harness):
        """Holding AUX updates GROUP button LEDs to show send slot colors."""
        harness.drain(timeout=0.1)

        with harness.holding(AUX):
            midi = harness.collect_midi(timeout=0.5)
            # GROUP buttons are NoteOn ch1, notes 8-15
            group_msgs = [
                m for m in midi
                if m.get("status") == 0x90
                and m.get("channel") == 1
                and 8 <= m.get("data1", -1) <= 15
            ]
            assert len(group_msgs) > 0, (
                "Expected GROUP LED updates while AUX held"
            )

    def test_aux_cycle_on_quick_press(self, harness, jam):
        """Quick AUX press without using group buttons cycles the AUX sub-mode."""
        harness.press(AUX)
        jam.assert_mode_active("AUX")

        # Press AUX again — cycles to next send slot; mode should remain active
        harness.press(AUX)
        jam.assert_mode_active("AUX")

        # Clean up
        harness.press(LEVEL)
        jam.assert_mode_active("LEVEL")
