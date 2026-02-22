"""OSC client for MonsterJam's state server.

Provides direct mode state queries via OSC, replacing log-based assertions.
Uses oscsend.py/osclisten.py from the harness tools (same as HarnessClient).
"""

import os
import queue
import subprocess
import sys
import threading
import time

_HARNESS_TOOLS = os.path.expanduser("~/work/bitwig/bitwig-harness/tools")
_OSCSEND = os.path.join(_HARNESS_TOOLS, "oscsend.py")
_OSCLISTEN = os.path.join(_HARNESS_TOOLS, "osclisten.py")


class JamClient:
    """OSC client for querying MonsterJam internal state."""

    def __init__(self, jam_host="localhost", jam_port=9200, reply_port=9201):
        self.jam_host = jam_host
        self.jam_port = jam_port
        self.reply_port = reply_port

        self._listener_proc = None
        self._reader_thread = None
        self._stop_event = threading.Event()
        self._queue = queue.Queue()

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def connect(self):
        """Start listener and register with MonsterJam's OSC server."""
        self._stop_event.clear()

        self._listener_proc = subprocess.Popen(
            [sys.executable, _OSCLISTEN, str(self.reply_port)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )

        self._reader_thread = threading.Thread(target=self._read_loop, daemon=True)
        self._reader_thread.start()

        # Give the listener time to bind
        time.sleep(0.3)

        # Register with MonsterJam
        self._osc_send("/mj/connect", ("i", str(self.reply_port)))
        time.sleep(0.3)

    def disconnect(self):
        """Unregister and stop listener."""
        try:
            self._osc_send("/mj/disconnect", ("i", str(self.reply_port)))
        except Exception:
            pass
        self._stop_event.set()
        if self._listener_proc:
            self._listener_proc.terminate()
            self._listener_proc.wait(timeout=3)
            self._listener_proc = None

    # ------------------------------------------------------------------
    # Sending
    # ------------------------------------------------------------------

    def _osc_send(self, address, *type_value_pairs):
        cmd = [
            sys.executable, _OSCSEND,
            self.jam_host, str(self.jam_port),
            address,
        ]
        for t, v in type_value_pairs:
            cmd.extend([t, str(v)])
        subprocess.run(cmd, check=True, timeout=5)

    # ------------------------------------------------------------------
    # Mode state queries
    # ------------------------------------------------------------------

    def mode_state(self, timeout=2.0):
        """Query current mode state.

        Returns (active: list[str], occulted: list[str]).
        """
        # Drain stale replies
        while not self._queue.empty():
            try:
                self._queue.get_nowait()
            except queue.Empty:
                break

        self._osc_send("/mj/mode/state")

        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError("Timed out waiting for /mj/mode/state reply")
            try:
                msg = self._queue.get(timeout=min(remaining, 0.1))
                if msg.get("address") == "/mj/mode/state":
                    args = msg.get("args", [])
                    active_str = args[0] if len(args) > 0 else ""
                    occulted_str = args[1] if len(args) > 1 else ""
                    active = [s for s in active_str.split(",") if s]
                    occulted = [s for s in occulted_str.split(",") if s]
                    return active, occulted
            except queue.Empty:
                continue

    def assert_mode_active(self, mode_id, timeout=2.0):
        """Poll mode_state() until mode_id appears in active list."""
        deadline = time.monotonic() + timeout
        last_active = []
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError(
                    f"Mode '{mode_id}' not active after {timeout}s "
                    f"(active: {last_active})"
                )
            try:
                active, _ = self.mode_state(timeout=min(remaining, 1.0))
                last_active = active
                if mode_id in active:
                    return active
            except TimeoutError:
                continue

    def assert_mode_inactive(self, mode_id, timeout=2.0):
        """Poll mode_state() until mode_id is absent from active list."""
        deadline = time.monotonic() + timeout
        last_active = []
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError(
                    f"Mode '{mode_id}' still active after {timeout}s "
                    f"(active: {last_active})"
                )
            try:
                active, _ = self.mode_state(timeout=min(remaining, 1.0))
                last_active = active
                if mode_id not in active:
                    return active
            except TimeoutError:
                continue

    def assert_mode_occulted(self, mode_id, timeout=2.0):
        """Poll mode_state() until mode_id appears in occulted list."""
        deadline = time.monotonic() + timeout
        last_occulted = []
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError(
                    f"Mode '{mode_id}' not occulted after {timeout}s "
                    f"(occulted: {last_occulted})"
                )
            try:
                _, occulted = self.mode_state(timeout=min(remaining, 1.0))
                last_occulted = occulted
                if mode_id in occulted:
                    return occulted
            except TimeoutError:
                continue

    def control_submode(self, timeout=2.0):
        """Query the current CONTROL submode.

        Returns (index: int, name: str).
        """
        # Drain stale replies
        while not self._queue.empty():
            try:
                self._queue.get_nowait()
            except queue.Empty:
                break

        self._osc_send("/mj/control/submode")

        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError("Timed out waiting for /mj/control/submode reply")
            try:
                msg = self._queue.get(timeout=min(remaining, 0.1))
                if msg.get("address") == "/mj/control/submode":
                    args = msg.get("args", [])
                    idx = args[0] if len(args) > 0 else -1
                    name = args[1] if len(args) > 1 else ""
                    return idx, name
            except queue.Empty:
                continue

    def assert_control_submode(self, name, timeout=2.0):
        """Poll control_submode() until the submode name matches."""
        deadline = time.monotonic() + timeout
        last_name = ""
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError(
                    f"CONTROL submode '{name}' not active after {timeout}s "
                    f"(current: '{last_name}')"
                )
            try:
                _, last_name = self.control_submode(timeout=min(remaining, 1.0))
                if last_name == name:
                    return last_name
            except TimeoutError:
                continue

    # ------------------------------------------------------------------
    # Internal
    # ------------------------------------------------------------------

    def _read_loop(self):
        proc = self._listener_proc
        while not self._stop_event.is_set() and proc.poll() is None:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if not line:
                continue
            try:
                msg = self._parse_osc_line(line)
            except (ValueError, IndexError):
                continue
            if msg:
                try:
                    self._queue.put_nowait(msg)
                except queue.Full:
                    pass

    @staticmethod
    def _parse_osc_line(line):
        """Parse osclisten.py output: '/address ,types arg1 arg2 ...'"""
        parts = line.split(None, 2)
        if len(parts) < 2:
            return None
        address = parts[0]
        type_tag = parts[1]
        args_str = parts[2] if len(parts) > 2 else ""

        args = []
        if args_str:
            type_chars = type_tag[1:]  # skip leading comma
            tokens = args_str.split()
            tok_idx = 0
            for i, tc in enumerate(type_chars):
                if tok_idx >= len(tokens):
                    break
                if tc in ("i", "h"):
                    args.append(int(tokens[tok_idx]))
                    tok_idx += 1
                elif tc in ("f", "d"):
                    args.append(float(tokens[tok_idx]))
                    tok_idx += 1
                elif tc == "s":
                    # Count fixed-size args remaining after this string
                    remaining_fixed = sum(1 for t in type_chars[i + 1:] if t != "s")
                    remaining_strings = sum(1 for t in type_chars[i + 1:] if t == "s")
                    tokens_needed_after = remaining_fixed + remaining_strings
                    available = len(tokens) - tok_idx - tokens_needed_after
                    if available < 1:
                        args.append("")
                    else:
                        args.append(" ".join(tokens[tok_idx:tok_idx + available]))
                        tok_idx += available
                else:
                    args.append(tokens[tok_idx])
                    tok_idx += 1

        return {"address": address, "type_tag": type_tag, "args": args}
