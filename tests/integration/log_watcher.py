"""Bitwig log monitor for MonsterJam internal state.

Tails the Bitwig Studio log file and filters for lines with the MJ prefix,
providing blocking waiters for mode activations and other internal events.

Requires MonsterJam "Verbose console output" to be enabled in Bitwig.
"""

import os
import re
import threading
import time


_DEFAULT_LOG_PATH = os.path.expanduser(
    "~/Library/Logs/Bitwig/BitwigStudio.log"
)


class LogWatcher:
    """Tail and filter the Bitwig log for MonsterJam events."""

    def __init__(self, log_path=None):
        self.log_path = log_path or _DEFAULT_LOG_PATH
        self._stop_event = threading.Event()
        self._reader_thread = None
        self._lines = []
        self._lines_lock = threading.Lock()
        self._new_line_event = threading.Event()

    def start(self):
        """Seek to end of file and begin tailing."""
        self._stop_event.clear()
        self._lines.clear()
        self._reader_thread = threading.Thread(target=self._tail_loop, daemon=True)
        self._reader_thread.start()

    def stop(self):
        """Stop tailing."""
        self._stop_event.set()
        if self._reader_thread:
            self._reader_thread.join(timeout=3)
            self._reader_thread = None

    def wait_for(self, pattern, timeout=2.0):
        """Wait for a log line matching regex pattern. Returns the line.

        Searches both already-captured lines and new lines as they arrive.
        """
        compiled = re.compile(pattern)
        deadline = time.monotonic() + timeout

        while True:
            # Check all captured lines
            with self._lines_lock:
                for line in self._lines:
                    if compiled.search(line):
                        return line

            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(
                    f"Timed out waiting for log pattern: {pattern}"
                )

            # Wait for new lines to arrive
            self._new_line_event.clear()
            self._new_line_event.wait(timeout=min(remaining, 0.1))

    def lines_since_start(self):
        """All MJ log lines captured since start()."""
        with self._lines_lock:
            return list(self._lines)

    def clear(self):
        """Clear captured lines (useful between tests)."""
        with self._lines_lock:
            self._lines.clear()

    def assert_activated(self, mode_id, timeout=2.0):
        """Assert that a mode was activated.

        Matches patterns like:
          "activating node {mode_id}:"
          "-- activated {mode_id} ---"
        """
        pattern = rf"(activating node {re.escape(mode_id)}:|-- activated {re.escape(mode_id)} ---)"
        return self.wait_for(pattern, timeout=timeout)

    def assert_deactivated(self, mode_id, timeout=2.0):
        """Assert that a mode was deactivated.

        Matches patterns like:
          "deactivating node {mode_id}:"
          "-- deactivated {mode_id} ---"
        """
        pattern = rf"(deactivating node {re.escape(mode_id)}:|-- deactivated {re.escape(mode_id)} ---)"
        return self.wait_for(pattern, timeout=timeout)

    # ------------------------------------------------------------------
    # Internal
    # ------------------------------------------------------------------

    def _tail_loop(self):
        """Tail the log file, capturing MJ-prefixed lines."""
        try:
            with open(self.log_path, "r") as f:
                # Seek to end
                f.seek(0, 2)

                while not self._stop_event.is_set():
                    line = f.readline()
                    if not line:
                        time.sleep(0.05)
                        continue

                    line = line.rstrip("\n")

                    # Filter for MJ lines — the Bitwig log prefix varies,
                    # but MonsterJam logs contain "MJ" or the extension name
                    if self._is_mj_line(line):
                        with self._lines_lock:
                            self._lines.append(line)
                        self._new_line_event.set()
        except FileNotFoundError:
            pass  # Log file doesn't exist yet

    @staticmethod
    def _is_mj_line(line):
        """Check if a log line is from MonsterJam."""
        # MJ logs typically contain "MJ" prefix or MonsterJam markers.
        # Bitwig log format: "timestamp [thread] message"
        # MonsterJam uses ext.host.println() which produces lines containing
        # the extension output directly.
        return "MJ" in line or "KeyMaster" in line or "activating node" in line or "deactivating node" in line
