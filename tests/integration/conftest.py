"""pytest fixtures for MonsterJam integration tests."""

import os
import subprocess
import time
import warnings

import pytest

from harness_client import HarnessClient
from jam_client import JamClient
from jam_midi import CLEAR, DUPLICATE, LEVEL, SONG
from log_watcher import LogWatcher

# Destructive modifiers released before each test.  CLEAR+pad deletes clips,
# DUPLICATE+pad duplicates them.  Other modifiers (SOLO, MUTE, etc.) only
# change modes and are safe to leave stuck.
_DESTRUCTIVE_MODIFIERS = [CLEAR, DUPLICATE]

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_PROJECT_FILE = os.path.join(_TEST_DIR, "test-project.dawproject")
_FORCE_RELOAD = os.environ.get("FORCE_RELOAD", "").lower() in ("1", "true", "yes")
_NO_AUTO_OPEN = os.environ.get("NO_AUTO_OPEN", "").lower() in ("1", "true", "yes")


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


def _engine_health_check(harness, jam):
    """Verify MonsterJam and the Bitwig audio engine are responsive.

    Phase 1 (~0.5s): proves the MonsterJam extension is alive.
    Phase 2 (~2s): sends a transport play/stop round-trip to confirm the
    audio engine is running.
    """
    # Phase 1: MonsterJam liveness
    try:
        jam.mode_state(timeout=3.0)
    except TimeoutError:
        pytest.exit(
            "MonsterJam not responding on port 9200. "
            "Ensure Bitwig is running with MonsterJam enabled "
            "and 'Verbose console output' is on.",
            returncode=1,
        )

    # Phase 2: Audio engine liveness
    harness.drain(timeout=0.1)
    harness.send_command("/transport/play")
    try:
        harness.wait_for(
            "/state/transport",
            predicate=lambda m: m["state"] == "playing",
            timeout=3.0,
        )
    except TimeoutError:
        pytest.exit(
            "Audio engine not running — /transport/play had no effect. "
            "Activate the audio engine in Bitwig (power icon in status bar).",
            returncode=1,
        )
    finally:
        harness.send_command("/transport/stop")
        harness.drain(timeout=0.3)


@pytest.fixture(scope="session", autouse=True)
def _check_engine(harness, jam):
    _engine_health_check(harness, jam)


EXPECTED_PROJECT = "test-project"


@pytest.fixture(scope="session")
def project_name(harness, jam, _check_engine):
    """Ensure the test project is loaded, opening it if not already open.

    Bitwig cannot close projects programmatically, so if a *different*
    project is already open we open the test project alongside it.  The
    harness observes whichever project is active, so this works as long
    as the test project ends up in the foreground.

    Set FORCE_RELOAD=1 to reimport even when the project name already
    matches (useful after updating the .dawproject file).
    Set NO_AUTO_OPEN=1 to skip opening and return the current project name.

    Returns the verified project name.
    """
    if _NO_AUTO_OPEN:
        state = harness.last_state
        return state.get("/state/project", {}).get("name", "")

    state = harness.last_state
    name = state.get("/state/project", {}).get("name", "")
    if name == EXPECTED_PROJECT and not _FORCE_RELOAD:
        return name

    # The test project might already be open but not in the foreground.
    # First try waiting briefly to see if we get a project state update
    # (e.g. the harness hasn't caught up yet).
    try:
        msg = harness.wait_for(
            r"/state/project",
            predicate=lambda m: m.get("name") == EXPECTED_PROJECT,
            timeout=2.0,
        )
        if not _FORCE_RELOAD:
            harness.drain(timeout=0.3)
            return msg["name"]
    except TimeoutError:
        pass

    # Project is genuinely not active — open it.
    current_name = harness.last_state.get("/state/project", {}).get("name", "")
    if current_name and current_name != EXPECTED_PROJECT:
        warnings.warn(
            f"A different project is active ('{current_name}'). "
            f"Opening '{EXPECTED_PROJECT}' alongside it.",
            stacklevel=2,
        )
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
        _engine_health_check(harness, jam)
        return msg["name"]
    except TimeoutError:
        # Fall through — skip fixture will handle it per-test
        state = harness.last_state
        return state.get("/state/project", {}).get("name", "")


@pytest.fixture(scope="session")
def _check_clips(harness, project_name):
    """Verify the test project has clips.  Runs once per session.

    Launches scene 0 via OSC and checks for a playing clip.  If no clip
    state arrives, the project is likely missing clips (e.g. they were
    accidentally deleted by a previous test run).
    """
    if project_name != EXPECTED_PROJECT:
        return  # wrong project — _skip_known_project_if_wrong handles it

    harness.drain(timeout=0.1)
    harness.send_command("/scene/launch", ("i", "0"))
    try:
        harness.wait_for(
            "/state/clip",
            predicate=lambda m: m.get("has_content") == 1,
            timeout=5.0,
        )
    except TimeoutError:
        pytest.exit(
            "No clips found in test project.  Clips may have been deleted "
            "by a previous test run.  Reimport: open test-project.dawproject "
            "in Bitwig, or run with FORCE_RELOAD=1.",
            returncode=1,
        )
    finally:
        harness.send_command("/transport/stop")
        harness.drain(timeout=0.3)


@pytest.fixture(scope="session")
def _check_track0_devices(harness, project_name):
    """Verify track 0 has devices.  Runs once per session.

    Many device tests assume track 0 has at least one device with remote
    control parameters.  Fail fast with a clear message instead of letting
    individual tests time out.
    """
    if project_name != EXPECTED_PROJECT:
        return

    harness.send_command("/track/select", ("i", "0"))
    harness.send_command("/device/select", ("i", "0"))
    try:
        harness.wait_for("/state/device", timeout=5.0)
    except TimeoutError:
        pytest.exit(
            "Track 0 has no devices — check test project.  "
            "Device tests require at least one device with remote controls on track 0.",
            returncode=1,
        )
    harness.drain(timeout=0.2)


@pytest.fixture(autouse=True)
def _skip_known_project_if_wrong(request, project_name, _check_clips, _check_track0_devices):
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
    # Release destructive modifiers to prevent stuck-modifier cascades.
    # If a previous test failed while holding CLEAR, every subsequent
    # pad press would delete a clip.
    for btn in _DESTRUCTIVE_MODIFIERS:
        harness.release(btn)
    # Drain after releasing destructive modifiers so the releases are
    # processed before any subsequent button presses in this fixture.
    harness.drain(timeout=0.1)

    # Stop transport to ensure consistent starting state
    harness.send_command("/transport/stop")

    # Reset track bank to position 0 and select track 0
    harness.send_command("/track/bank/scroll", ("i", "0"))
    harness.send_command("/track/select", ("i", "0"))

    # Deactivate sceneCycle if active
    modes = jam.mode_state(timeout=1.0)
    active = modes.get("active", []) if isinstance(modes, dict) else []

    # Fail loudly if CLEAR is still active despite the release above —
    # a stuck CLEAR turns every subsequent pad press into a clip delete.
    if "CLEAR" in active:
        # Try once more: explicit release + drain
        harness.release(CLEAR)
        harness.drain(timeout=0.2)
        modes = jam.mode_state(timeout=1.0)
        active = modes.get("active", []) if isinstance(modes, dict) else []
        assert "CLEAR" not in active, (
            "CLEAR modifier is stuck active after two release attempts — "
            "aborting to prevent clip deletion"
        )

    if "sceneCycle" in active:
        harness.press(SONG)
        harness.drain(timeout=0.1)

    # Reset strip mode to LEVEL if not already active
    if "LEVEL" not in active:
        harness.press(LEVEL)
        harness.drain(timeout=0.1)

    # Drain any queued messages from previous test
    harness.drain(timeout=0.1)
    # Clear log lines from previous test
    log.clear()
