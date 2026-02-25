"""Tier 2 — Device navigation and remote controls tests.

Requires a known Bitwig project with at least one device that has
remote control parameters.

Mark: known_project
"""

import pytest

pytestmark = pytest.mark.known_project


@pytest.fixture(autouse=True, scope="class")
def select_track_0(harness):
    """Ensure cursor is on track 0 (not master) for device tests."""
    harness.send_command("/track/select", ("i", "0"))
    harness.drain(timeout=0.2)


class TestDevices:

    def test_device_remote_page_next(self, harness):
        """Advancing the remote control page increments the page index."""
        harness.send_command("/device/select", ("i", "0"))
        harness.send_command("/remote_control/page/select", ("i", "0"))
        msg0 = harness.wait_for(
            "/state/remote_control/page",
            predicate=lambda m: m.get("index") == 0,
            timeout=2.0,
        )
        if msg0.get("count", 999) < 2:
            pytest.skip("Device has fewer than 2 remote pages")
        harness.drain(timeout=0.1)

        harness.send_command("/remote_control/page/next")

        msg = harness.wait_for(
            "/state/remote_control/page",
            predicate=lambda m: m.get("index", -1) == 1,
            timeout=2.0,
        )
        assert msg["index"] == 1

    def test_device_remote_set(self, harness):
        """Setting a remote control parameter updates its value."""
        harness.send_command("/device/select", ("i", "0"))
        harness.send_command("/remote_control/page/select", ("i", "0"))
        harness.drain(timeout=0.1)

        target_value = 0.5
        harness.send_command("/remote_control/set", ("i", "0"), ("f", str(target_value)))

        msg = harness.wait_for(
            "/state/remote_control/param",
            predicate=lambda m: (
                m.get("index") == 0
                and abs(m.get("value", -1) - target_value) < 0.05
            ),
            timeout=2.0,
        )
        assert abs(msg["value"] - target_value) < 0.05

    def test_device_select(self, harness):
        """Selecting a device by index updates the cursor device."""
        harness.send_command("/device/select", ("i", "1"))

        msg = harness.wait_for(
            "/state/device",
            predicate=lambda m: m.get("index") == 1,
            timeout=2.0,
        )
        assert msg["index"] == 1
