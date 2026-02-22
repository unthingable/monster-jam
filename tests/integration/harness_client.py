"""OSC client for the bitwig-harness.

Manages connection lifecycle, sends MIDI/commands, and provides blocking
state waiters.  Uses oscsend.py/osclisten.py from the harness tools dir
as subprocesses.
"""

import os
import queue
import re
import subprocess
import sys
import threading
import time

# Locate harness tools relative to this project
_HARNESS_TOOLS = os.path.expanduser("~/work/bitwig/bitwig-harness/tools")
_OSCSEND = os.path.join(_HARNESS_TOOLS, "oscsend.py")
_OSCLISTEN = os.path.join(_HARNESS_TOOLS, "osclisten.py")


def _parse_typed_tokens(type_chars, tokens):
    """Parse space-split tokens according to OSC type tag characters.

    Handles string args that contain spaces by computing how many tokens
    each string arg should consume: fixed types (i, f, h, d) always take
    exactly one token, so strings get whatever is left over.
    """
    args = []
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
            # Count how many single-token (fixed) args remain after this one
            remaining_fixed = sum(1 for t in type_chars[i + 1:] if t != "s")
            remaining_strings = sum(1 for t in type_chars[i + 1:] if t == "s")
            tokens_needed_after = remaining_fixed + remaining_strings
            # This string consumes all tokens except those reserved for later args
            available = len(tokens) - tok_idx - tokens_needed_after
            if available < 1:
                available = 1
            args.append(" ".join(tokens[tok_idx:tok_idx + available]))
            tok_idx += available
        else:
            args.append(tokens[tok_idx])
            tok_idx += 1
    return args


class HarnessClient:
    """Bidirectional OSC client wrapping the harness tools."""

    def __init__(self, harness_host="localhost", harness_port=9000, reply_port=9002):
        self.harness_host = harness_host
        self.harness_port = harness_port
        self.reply_port = reply_port

        self._listener_proc = None
        self._reader_thread = None
        self._stop_event = threading.Event()
        self._state = {}       # address -> latest parsed message dict
        self._state_lock = threading.Lock()
        self._queue = queue.Queue()  # all incoming messages for wait_for

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def connect(self):
        """Start the listener subprocess and register with the harness."""
        self._stop_event.clear()

        # Start osclisten.py to receive state updates
        self._listener_proc = subprocess.Popen(
            [sys.executable, _OSCLISTEN, str(self.reply_port)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,  # line-buffered
        )

        # Reader thread to parse incoming OSC messages
        self._reader_thread = threading.Thread(target=self._read_loop, daemon=True)
        self._reader_thread.start()

        # Give the listener a moment to bind
        time.sleep(0.3)

        # Register with harness — triggers snapshot
        self._osc_send("/connect", ("i", str(self.reply_port)))

        # Wait a beat for the snapshot to arrive
        time.sleep(0.5)

        return dict(self._state)

    def disconnect(self):
        """Unregister from harness and stop listener."""
        try:
            self._osc_send("/disconnect", ("i", str(self.reply_port)))
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
        """Send an OSC message via oscsend.py.

        Each arg is a (type, value) tuple, e.g. ("i", "127").
        """
        cmd = [
            sys.executable, _OSCSEND,
            self.harness_host, str(self.harness_port),
            address,
        ]
        for t, v in type_value_pairs:
            cmd.extend([t, str(v)])
        subprocess.run(cmd, check=True, timeout=5)

    def send_midi(self, status_byte, data1, data2):
        """Send MIDI via the harness proxy.

        Accepts a full status byte (e.g. 0xB0 for CC ch0, 0x91 for NoteOn ch1).
        Decomposes into channel + status for the harness /midi/send format.
        """
        channel = status_byte & 0x0F
        status = status_byte & 0xF0
        self._osc_send(
            "/midi/send",
            ("i", str(channel)),
            ("i", str(status)),
            ("i", str(data1)),
            ("i", str(data2)),
        )

    def press(self, button, hold_ms=0):
        """Press a button (value=127), optional hold, then release (value=0).

        `button` is a (status_byte, data1) tuple from jam_midi.
        """
        status_byte, data1 = button
        self.send_midi(status_byte, data1, 127)
        if hold_ms > 0:
            time.sleep(hold_ms / 1000.0)
        self.send_midi(status_byte, data1, 0)

    def hold(self, button):
        """Press a button down (value=127) without releasing."""
        status_byte, data1 = button
        self.send_midi(status_byte, data1, 127)

    def release(self, button):
        """Release a button (value=0)."""
        status_byte, data1 = button
        self.send_midi(status_byte, data1, 0)

    def send_command(self, address, *type_value_pairs):
        """Send an arbitrary OSC command to the harness.

        Args can be (type, value) tuples or omitted for no-arg commands.
        Example: send_command("/transport/play")
                 send_command("/track/select", ("i", "2"))
        """
        self._osc_send(address, *type_value_pairs)

    # ------------------------------------------------------------------
    # Receiving / waiting
    # ------------------------------------------------------------------

    def wait_for(self, address_pattern, timeout=2.0, predicate=None):
        """Block until a message matching address_pattern (and optional predicate) arrives.

        Returns the parsed message dict, or raises TimeoutError.
        """
        deadline = time.monotonic() + timeout

        # Check already-received state first
        with self._state_lock:
            for addr, msg in self._state.items():
                if re.search(address_pattern, addr):
                    if predicate is None or predicate(msg):
                        return msg

        # Wait for new messages
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(
                    f"Timed out waiting for {address_pattern} "
                    f"(last state: {self._state.get(address_pattern, 'none')})"
                )
            try:
                msg = self._queue.get(timeout=min(remaining, 0.1))
                if re.search(address_pattern, msg.get("address", "")):
                    if predicate is None or predicate(msg):
                        return msg
            except queue.Empty:
                # Re-check state in case we missed it
                with self._state_lock:
                    for addr, msg in self._state.items():
                        if re.search(address_pattern, addr):
                            if predicate is None or predicate(msg):
                                return msg

    def drain(self, timeout=0.5):
        """Collect all messages for timeout seconds, return list."""
        messages = []
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                break
            try:
                msg = self._queue.get(timeout=min(remaining, 0.05))
                messages.append(msg)
            except queue.Empty:
                continue
        return messages

    def collect_midi(self, timeout=1.0):
        """Collect all /midi/in messages for timeout seconds.

        Unlike wait_for, this doesn't discard non-matching messages.
        Useful when you need to assert multiple conditions on a burst
        of MIDI messages.
        """
        all_msgs = self.drain(timeout=timeout)
        return [m for m in all_msgs if m.get("address") == "/midi/in"]

    @property
    def last_state(self):
        """Most recent state per address."""
        with self._state_lock:
            return dict(self._state)

    # ------------------------------------------------------------------
    # Internal
    # ------------------------------------------------------------------

    def _read_loop(self):
        """Read lines from osclisten.py stdout and parse them."""
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
            except (ValueError, IndexError) as e:
                # Skip unparseable lines rather than crashing the reader
                continue
            if msg:
                with self._state_lock:
                    self._state[msg["address"]] = msg
                try:
                    self._queue.put_nowait(msg)
                except queue.Full:
                    pass

    @staticmethod
    def _parse_osc_line(line):
        """Parse a line from osclisten.py output.

        Format: "/address ,types arg1 arg2 ..."
        """
        parts = line.split(None, 2)  # address, type_tag, rest
        if len(parts) < 2:
            return None

        address = parts[0]
        type_tag = parts[1]
        args_str = parts[2] if len(parts) > 2 else ""

        # Parse args based on type tag.
        # Challenge: string args may contain spaces, but osclisten outputs
        # space-separated values.  Strategy: parse fixed-size types (i, f)
        # from the right to determine where string boundaries fall.
        args = []
        if args_str:
            tokens = args_str.split()
            type_chars = type_tag[1:]  # skip leading comma
            args = _parse_typed_tokens(type_chars, tokens)

        msg = {"address": address, "type_tag": type_tag, "args": args}

        # Add semantic keys for known state messages
        if address == "/state/transport" and len(args) >= 1:
            msg["state"] = args[0]
        elif address == "/state/cursor_track" and len(args) >= 2:
            msg["index"] = args[0]
            msg["name"] = args[1]
        elif address == "/state/device" and len(args) >= 2:
            msg["name"] = args[0]
            msg["index"] = args[1]
        elif address == "/state/track" and len(args) >= 4:
            msg["bank_index"] = args[0]
            msg["name"] = args[1]
            msg["position"] = args[2]
            msg["type"] = args[3]
        elif address == "/state/remote_control/page" and len(args) >= 3:
            msg["name"] = args[0]
            msg["index"] = args[1]
            msg["count"] = args[2]
        elif address == "/state/remote_control/param" and len(args) >= 3:
            msg["index"] = args[0]
            msg["name"] = args[1]
            msg["value"] = args[2]
        elif address == "/state/clip" and len(args) >= 5:
            msg["track"] = args[0]
            msg["scene"] = args[1]
            msg["has_content"] = args[2]
            msg["is_playing"] = args[3]
            msg["is_recording"] = args[4]
        elif address == "/midi/in" and len(args) >= 4:
            msg["channel"] = args[0]
            msg["status"] = args[1]
            msg["data1"] = args[2]
            msg["data2"] = args[3]

        return msg
