"""pytest fixtures for MonsterJam integration tests."""

import time

import pytest

from harness_client import HarnessClient
from jam_client import JamClient
from log_watcher import LogWatcher


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
