"""Maschine JAM MIDI mapping constants.

All constants are (status_byte, data1) tuples representing the full MIDI
status byte (including channel) and the CC/note number.  To send via the
harness MIDI proxy, decompose as:
    channel = status_byte & 0x0F
    status  = status_byte & 0xF0
"""

# ---------------------------------------------------------------------------
# Status bytes
# ---------------------------------------------------------------------------
CC_CH0 = 0xB0       # 176 — Control Change, channel 0
NOTE_ON_CH0 = 0x90  # 144 — Note On, channel 0
NOTE_ON_CH1 = 0x91  # 145 — Note On, channel 1

# ---------------------------------------------------------------------------
# CC buttons (channel 0)
# ---------------------------------------------------------------------------

# Transport row
PLAY = (CC_CH0, 108)
RECORD = (CC_CH0, 109)
TEMPO = (CC_CH0, 110)
SOLO = (CC_CH0, 111)
MUTE = (CC_CH0, 112)
GRID = (CC_CH0, 113)
LEFT = (CC_CH0, 107)   # Arrow Left
RIGHT = (CC_CH0, 104)  # Arrow Right

# Left column
SONG = (CC_CH0, 30)       # BtnArrange
STEP = (CC_CH0, 31)
PAD_MODE = (CC_CH0, 32)
CLEAR = (CC_CH0, 95)
DUPLICATE = (CC_CH0, 96)
NOTE_REPEAT = (CC_CH0, 94)  # BtnArpRepeat

MACRO = (CC_CH0, 90)
LEVEL = (CC_CH0, 91)
AUX = (CC_CH0, 92)
CONTROL = (CC_CH0, 97)
AUTO = (CC_CH0, 98)

# Right column
MASTER = (CC_CH0, 60)   # BtnMst
GROUP_BTN = (CC_CH0, 61)  # BtnGrp (distinct from GROUP track buttons)
IN1 = (CC_CH0, 62)
CUE = (CC_CH0, 63)

BROWSE = (CC_CH0, 44)
PERFORM = (CC_CH0, 45)
NOTES = (CC_CH0, 46)    # BtnVariation
LOCK = (CC_CH0, 47)
TUNE = (CC_CH0, 48)
SWING = (CC_CH0, 49)
SELECT = (CC_CH0, 80)

# D-pad
DPAD_UP = (CC_CH0, 40)     # BtnDpad1
DPAD_DOWN = (CC_CH0, 41)   # BtnDpad3
DPAD_LEFT = (CC_CH0, 42)   # BtnDpad4
DPAD_RIGHT = (CC_CH0, 43)  # BtnDpad2

# Encoder
ENCODER_TURN = (CC_CH0, 86)   # Relative 2's complement, center=64
ENCODER_PUSH = (CC_CH0, 87)
ENCODER_TOUCH = (CC_CH0, 88)

# ---------------------------------------------------------------------------
# Note buttons — channel 1
# ---------------------------------------------------------------------------

# Group buttons A-H (notes 8-15 on channel 1)
GROUP = [(NOTE_ON_CH1, n) for n in range(8, 16)]

# Scene buttons 1-8 (notes 0-7 on channel 1)
SCENE = [(NOTE_ON_CH1, n) for n in range(0, 8)]

# ---------------------------------------------------------------------------
# Matrix pads — 8x8 grid (notes on channel 0)
#
# PAD[row][col] where row=0 is top (row 8 in hardware), col=0 is left (col A).
# The hardware matrix is column-major: column A = notes 22,30,38,...,78
# but JamSurface indexes as row × col, with row 1 = top.
# ---------------------------------------------------------------------------
PAD = [
    [(NOTE_ON_CH0, 22 + col + row * 8) for col in range(8)]
    for row in range(8)
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def decompose(button):
    """Decompose a (status_byte, data1) tuple into (channel, status, data1)."""
    status_byte, data1 = button
    return status_byte & 0x0F, status_byte & 0xF0, data1
