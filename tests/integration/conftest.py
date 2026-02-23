"""pytest fixtures for MonsterJam integration tests."""

import os
import subprocess
import time

import pytest

from harness_client import HarnessClient
from jam_client import JamClient
from log_watcher import LogWatcher

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_PROJECT_FILE = os.path.join(_TEST_DIR, "test-project.dawproject")


@pytest.fixture(scope="session")
def harness():
    """Connect to harness, yield client, disconnect on teardown."""
    client = HarnessClient(reply_port=9002)
    client.connect()
    yield client
    client.disconnect()


@pytest.fixture(scope="session")
def jam():
    """Connect to MonsterJam OSC server, yield client, disconnect on teardown."""
    client = JamClient(reply_port=9201)
    client.connect()
    yield client
    client.disconnect()


@pytest.fixture(scope="session")
def log():
    """Start log watcher, yield it, stop on teardown."""
    watcher = LogWatcher()
    watcher.start()
    yield watcher
    watcher.stop()


EXPECTED_PROJECT = "test-project"


@pytest.fixture(scope="session")
def project_name(harness):
    """Ensure the test project is loaded, opening it if necessary.

    Returns the verified project name.  If the project can't be loaded
    within the timeout, returns whatever name is currently active (the
    per-test skip fixture will handle it).
    """
    state = harness.last_state
    name = state.get("/state/project", {}).get("name", "")
    if name == EXPECTED_PROJECT:
        return name

    # Wrong project (or none) — try to open it
    subprocess.Popen(["open", _PROJECT_FILE])
    try:
        msg = harness.wait_for(
            r"/state/project",
            predicate=lambda m: m.get("name") == EXPECTED_PROJECT,
            timeout=15.0,
        )
        # Extra settle time for Bitwig to finish loading
        time.sleep(2.0)
        harness.drain(timeout=0.5)
        return msg["name"]
    except TimeoutError:
        # Fall through — skip fixture will handle it per-test
        state = harness.last_state
        return state.get("/state/project", {}).get("name", "")


@pytest.fixture(autouse=True)
def _skip_known_project_if_wrong(request, project_name):
    """Skip known_project tests if the wrong project is loaded."""
    marker = request.node.get_closest_marker("known_project")
    if marker is not None and project_name != EXPECTED_PROJECT:
        pytest.skip(
            f"Wrong project loaded: expected '{EXPECTED_PROJECT}', "
            f"got '{project_name}'"
        )


@pytest.fixture(autouse=True)
def settle(harness, jam, log):
    """Ensure clean state before each test."""
    # Stop transport to ensure consistent starting state
    harness.send_command("/transport/stop")
    time.sleep(0.2)
    # Drain any queued messages from previous test
    harness.drain(timeout=0.2)
    # Clear log lines from previous test
    log.clear()
