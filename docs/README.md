# MonsterJam

Support for Native Instruments Maschine JAM. 

This only works on Mac and Windows (because it needs NI drivers for the Jam controller).

# Installation

1. Drop `MonsterJam.bwextension` into your `Bitwig Extensions` folder
1. Ensure correct controller mapping is loaded on Jam
1. If Bitwig did not autodetect, add the `MonsterJam` controller device and map the MIDI ports

Maschine JAM must be in MIDI mode: press **SHIFT+HEADPHONES (MIDI)** buttons.

## Controller mapping

MonsterJam is flexible about controller mappings. There are two ways 
to go about this.

### Default

Use NI Controller Editor to load the included `Bitwig Studio ext.ncmj` mapping.

### Bring your own mapping

Yes! Drop your own `*.ncmj` file into `Bitwig Extensions` directory and it should work!
If there are multiple `*.ncmj` files MonsterJam will pick the first one.
This is convenient if you already have a mapping you're using for other reasons.

Obviously, if some controls are disabled in the controller mapping they
will not function. Lights may behave incorrectly if they are set to 
"For MIDI Out" instead of "For MIDI In". 
Also, MonsterJam expects controls to be mapped to
notes or CC. If you do something unorthodox like pitch bend or program change
MonsterJam will probably ignore it or even fail to load.

# Overview

MonsterJam focuses primarily on essential performance features, less on content creation (though more features will be added later).
Implementation ideas borrowed from original script, Maschine and Moss 
(including some documentation snippets), among others.

Buttons labelled A-H are called "group" by Maschine, we'll call them "track" for simplicity.

**NOTE**: Key bindings are experimental and may change in future versions.

## Button combination order

Chorded buttons are sensitive to order. For example, **SHIFT+CONTROL** is not the same as **CONTROL+SHIFT**.

## Global

* **KNOB turn**: Move arranger playhead, jog through the project timeline. Hold **SHIFT** to adjust tempo in finer increments.
* **SONG**: **SuperScene** mode (see below) or "home" (return to default Clip Launcher view)
* **STEP**: Step Sequencer
* **CLEAR**: Use in combination with other buttons to delete a scene (scene buttons), clip (a pad in session mode) or track (group buttons).
* **DUPLICATE**: Combine with a scene pad (duplicate scene) or a track button (duplicate track). To copy clips in session mode keep the Duplicate button pressed; choose the source clip (it must be a clip with content, you can still select a different clip as the source); select the destination clip (this must be an empty clip, which can also be on a different track); release the Duplicate button.
* **SHIFT+DUPLICATE (DOUBLE)**: Double the content of the currently selected clip (not the clip itself).
* **SHIFT-PAD (top row)**: The buttons in the top row of clip matrix change to their alternate functions, as labeled.
* **SHIFT-PAD (second row)**: Additional functions and settings
  * **SHIFT-PAD 1**: Toggle hiding/showing disabled tracks
* **NOTE REPEAT**: Fill mode
* **MACRO**: Activate **Track Selector** and **user controls** mode (see **CONTROL**)

## Transport

* **PLAY**: Start/Stop playback
* **REC**: Toggle recording
* **SHIFT+PLAY** Restart or pause/continue (see **Settings**)
* **SHIFT+RECORD**: Toggle launcher overdub
* **SHIFT+PAGE LEFT**: Toggle the metronome
* **SHIFT+PAGE RIGHT**: Toggle transport loop
* **SHIFT+TEMPO**: Tap Tempo
* **TEMPO+KNOB turn**: change tempo
* **GRID**: Hold to change launch grid quantization with SCENE buttons (see Launch grid quantization below)
* **SOLO**: Solo mode, press track buttons to enable/disable solo. Keep holding SOLO button to automatically 
  disable Solo mode when released.
* **MUTE**: Mute mode, same as Solo
* **AUTO**: Toggle arranger automation write. Flashing when automation override is active, press to restore.
* **SHIFT+AUTO**: Toggle launcher automation write

## Group navigation, track selection and manipulation

Track buttons are lit in the color of their respective tracks or blank if track does not exist. Use DPAD left/right arrows to scroll tracks or SHIFT-TRACK to select one of first 8 track pages.

* **TRACK(A-H)**: Select the tracks in the current page of the track bank. Track buttons will light up on MIDI events
from the track, for additional fun and profit (like Maschine).
* **MST**: Selects the Master track, or the parent group track if inside a group
* **TRACK(A-H)+...**: Hold track button for more functions:
  * **ARROW DOWN**: If this track is a group track, enter it. Arrow button is lit and flashing when this is possible.
  * **ARROW UP**: If we're currently in a group, exit out of it. Arrow button is lit and flashing when this is possible.
  * **ARROW LEFT**: Scroll bank left so that current track all the way to the right (to position 8 on Jam)
  * **ARROW RIGHT**: Scroll bank right so that current track all the way to the left (to position 1 on Jam)
  * **another TRACK**: Scroll by that much in that direction. Conceptually this is almost the same as dragging on a touchscreen.
  * **SOLO**: Solo/unsolo this track
  * **MUTE**: Mute/unmute this track
  * **REC**: Arm track for recording
* Doubleclick **TRACK(A-H)** on a group track to expand/collapse this group.
  
## Launching scenes: Scene (1-8) buttons and clip buttons

Scene buttons are lit in the color of their respective scenes or blank if scene does not exist. Use DPAD up/down arrows to scroll scenes and SHIFT-SCENE to select one of first 8 scene pages.

* **SCENE(1-8)**: Launch the scenes in the current page of the scene bank
* **(PAD)**: On a group track, launch the group clip as a scene

When inside a group, the SCENE buttons launch the group scenes, not the main ones.

## Clip Launcher

* **(PAD)** on empty clip: record new clip
* **(PAD)** on existing clip: start clip playback
* **(PAD)** on playing clip: stop clip playback
* **SELECT+(PAD)**: Select the clip without starting it. Alternatively, *long press* the pad.
  While **SELECT** is pressed the currently selected clip is WHITE.
* **CLEAR+(PAD)**: Delete the clip
* **DUPLICATE**: To duplicate a clip keep the duplicate button pressed; choose the source clip (it must be a clip with content, you can still select a different clip with content); select the destination clip (this must be an empty clip, which can also be on a different track); release the Duplicate button.

MonsterJam will attempt to retroactively launch the clip "on time" even if you are a little late triggering it, meaning you don't have to always "cue" it up in advance and let your musician reflexes take over. Note that this only works as well as Bitwig's own "continue" play mode allows, i.e. there must be another clip playing on that track. See **Launch tolerance** setting.

## Page navigation

* The arrow keys scroll the grid by blocks of 8 tracks/scenes. Keys will light up if there is content to scroll to.
* **SHIFT+...** in regular mode: hold shift and then
  * Arrow keys: scroll by 1 track/scene (SHIFT function is flippable in settings)
  * **SCENE(1-8)**: directly select from the first 8 pages of scenes
  * **TRACK(1-8)**: directly select from the first 8 pages of tracks (if enabled in settings)
  * The currently selected scene/track page is **white**, available pages are **yellow**. If the view is currently between pages, two adjacent pages will be orange.
* **SHIFT+...** in SuperScene mode:
  * Track buttons and arrow keys same as above
  * **SCENE(1-8)**: select SuperScene page. Pages with contents are **cyan**, current page is **white**, page with last selected scene is **lime**.
  * **(PAD) (bottom row)**: scene page selector is preserved but is now on the bottom matrix row instead of SCENE buttons.
* **MACRO** lets you jump directly to one of 64 tracks using pad buttons
* **SONG** (hold): clip pads become page selectors, same logic as SHIFT+SCENE/TRACK

## Touch Strips

* **LEVEL**: Toggles between Volume and Panorama editing of 8 tracks. 
  If Volume is active and playback is started the VU of the tracks is displayed as well. 
  All strips are lit in their tracks' color.
  * Note: maximum slider range can be limited to values other than +6 dB, see **Settings**.
* **AUX**: Edit send levels for a specific send for all tracks. Strips are lit in corresponding Effect track colors.
* **AUX+TRACK(A-H)**: Hold **AUX** down to select from the first 8 Effect tracks. Track buttons are lit in corresponding 
  Effect track colors, currently selected send is in WHITE.
* **CONTROL**: Edit device remote controls
  * **PAGE LEFT/PAGE RIGHT**: Select previous/next device in the device chain
  * **CONTROL + PAGE LEFT/PAGE RIGHT**: Select previous/next parameter page in the current device. Page buttons light up 
    when there are pages to navigate to.
  * **CONTROL+SELECT**: Enable **Device Selector** mode (see below)
  * **CONTROL+MACRO**: Toggle between device controls (rainbow) and **user controls** (all red when mapped).
  * **LEFT+RIGHT**: Toggle Control Slice mode (see below)
* **CLEAR+strip**: Reset parameter value

### Fine adjustment

Hold **SHIFT** to reduce strip sensitivity and adjust in finer increments. Strips behave like relative controls. 
This works in all touchstrip modes except user controls, because their implementation sucks.

### Control Slice

Imagine the device controls laid out on a 8x8 knob controller, where each vertical column corresponds to the 8 device
parameters per track. In Control mode you control a single column at a time with the touch strips. 

In Control Slice mode you control a single row: one device parameter per each track. This is similar to how AUX sends
operate and you can use them as such. This mode is most useful if you put relevant parameters in the same position 
across your devices, e.g. parameter 1 -> filter cutoff, parameter 2 -> compressor attack, etc.

To activate: press **LEFT+RIGHT** in regular Control mode (not User Control, **MACRO** not lit).

To select a parameter: use **TRACK(1-8)** buttons. The currently selected parameter is lit in white. Note that selectors
are auto-gating: if you press and hold a track button, operate any touchstrip or just wait long enough, it will return
to the previously selected parameter when track button is released. This is useful for momentary adjustments.

To select a device and a page: whichever device was selected last on a track will be the device controlled in Slice mode.
Same goes for remote control pages within a device. You can use Device Selector to quickly select a device on each track.

# Step Sequencer (WIP)

* **STEP**: Toggle sequencer mode

Step sequencer settings are stored per track and saved with the project. Most steq sequencer submodes are auto-gating.

## Default layout

* **SCENE** (top): pattern pages. Currently selected page is bright white, currently playing page is dim white.
* **PAD** matrix: select and edit steps. When transport is playing the currently playing step is white (a chasing light).
* **DPAD** arrows up/down: scroll notes up or down by pages, left/right: by one step
* **KNOB**: scroll notes up or down by one
* **KNOB (push turn)**: scroll steps left/right
* **GRID**: activate grid selector
* **SHIFT+SOLO**: activate pattern length selector
* **NOTE**: activate Note/Velocity mode
* **TUNE**: adjust step parameters with sliders (see **Parameter adjustments**)
* **PERFORM (hold)**: change current MIDI channel via the lower right pad matrix quadrant (WIP)

### Scene buttons 

Display/select current patter page.

The number of pattern pages depends on both the step size and the current grid layout, there will be as many pages as necessary to access the entire length of the pattern (though only the first 8 are directly accessible).

### Pad matrix

* Press empty step (dark button) to create a step
* Press an existing step to clear
* Press and hold step to select it for editing (long press will not delete it)
* **SHIFT+NOTES** to toggle alternating note row colors

If a note spans more than one step, consecutive steps are lit in dim white.

#### Entering legato notes

Press and hold a pad to either select or create a step. Click another pad on the left to change note length in steps.

Extending a note all the way to the next step makes it legato.

## Grid mode and pattern pages

The step sequencer matrix is a windowed view of a clip, showing up to 64 steps at once.

*Step size* determines how many steps are in a clip (depends on clip length).

* **GRID**: activate grid selector to change grid configuration
* **SCENE**: set how many rows are given to each note
  * 1: 1 note per 8 rows
  * 2: 2 notes per 8 rows (4 rows per note)
  * 4: 4 notes per 8 rows (2 rows per note)
  * 8: 8 notes per 8 rows (1 row per note)
* **KNOB**: change step size

## Note/Velocity submode (WIP)

* **NOTE**: activate note/velocity selectors
* **NOTE (hold)**: display note page selectors (scene buttons)
* **NOTE+scene**: select a note page (each page is 16 notes)

When NOTE is on, the lower half of the pad matrix changes function and is solidly lit with the color of the clip. The mode itself is split in two:
* Left half: velocity selector
* Right half (numbered pads): note selector

### Velocity selector

Coarse velocity adjustment in 16 steps (1 step = 8 velocity steps, out of 128). Currently selected velocity step is white.

Velocity selector works with two different velocity values:
* Default velocity: newly entered note steps will use this
* Velocity of a specific note step: hold one or more note steps, then you can see/edit its velocity via the selector. If multiple steps are held, it's the last pressed step that takes precedence, and adjustments are made to all of them simultaneously.

### Note selector (WIP)

Play notes and focus step editor.

The 128 notes are divided in 8 pages of 16 notes each. The note selector is always showing one of those pages (think of the Chain scroller in Drum Maschine), which one depends on which page the topmost visible note falls into.

Pressing a pad:
* Plays the note
* Scrolls the step editor to that note (so that it's at the top row)

The button corresponding to the topmost visible note is bright white. If more notes are visible in the step view, their buttons will be dim white. Note that because the step grid is laid out top-down vertically (high notes on top, low on bottom, matching the clip view in the app) and the note buttons are left-right and down-up (as labeled on the Jam), this can look backwards. If this hurts your brain too much and you know of a better way, let me know.

Currently playing notes will flash, letting you see activity outside of the visible step grid.

**Press and hold NOTE** to see the page selector (SCENE buttons), higher notes are on the right like on a piano (if your brain isn't hurting yet, this is opposite of how scenes work with clips). Current page is bright white. Unlike Note selector that is fixed to a page, page selector will indicate if current note window straddles two pages, with next page button in dim white. Pressing a button always scrolls top note row to the top of the page.

Use note/page selectors together with knob scrolling for maximum nagivation.

## Parameter adjustments (WIP)

Press **TUNE** to access various note step parameters via sliders. (WIP: have to manually press one of LEVEL/AUX/CONTROL to reactivate after deactivating TUNE)

To edit parameter of one or more steps, hold them. When multiple steps are held, last pressed step takes precedence.

Fine adjustments with SHIFT are available.

Sliders:

1. Note velocity
1. Note release velocity
1. Note velocity spread
1. Note start offset (between the start of this step and the next one, in 128 increments)
1. Note duration
1. Pan
1. Timbre
1. Pressure

## Clip creation/selection behavior

Here "selected" means selected in the app.

When activating Step Sequencer:
* If the current active track has no existing clip, a new clip is created in the currently selected slot, or first slot if none are selected
* Either the selected clip or the first existing clip on a track is shown

# Modes and notes

## Auto-gating modes

An important UX behavior used in majority of modes in MonsterJam.

* A quick press on a mode button toggles the mode 
* Long press turns mode off upon release (the mode becomes momentary)
* Long press *with usage* (operate any of the mode's controls while holding the mode button) turns mode off upon release regardless of press duration

You can quickly drop into a mode, do something and drop back out with no additional keypresses. Very handy for quick navigation and performance.

Some specific modes use an inverted variant of this behavior (long press leaves mode on).

Some modes that are like this:
* SOLO, MUTE, TEMPO
* User control pages
* Control slice selectors
* etc. — if you think it should be auto-gating it probably is, and if not then let me know.

## Launch grid quantization

Hold **GRID** to select the current launch quantization with **SCENE** buttons. Buttons correspond to the following values:

* "8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16"

THe current setting is lit. Press the lit button to disable grid quantization altogether.

## PERFORM FX

Maschine native has a great "performance fx" mode, where dual-touch strips control both an effect parameter and effect routing
(e.g., an insert effect is enabled only when the strip is being touched). MonsterJam does something similar.

If a device has a remote control page called `MonsterFX`, MonsterJam will treat it specially:

* The page will be omitted from pages available to **CONTROL** mode
* When a strip is touched, a corresponding control on this page will be set to 1, otherwise 0

### Intended usage
Make a remote page called `MonsterFX` and fill it with buttons (macro knobs work too, but buttons make the most sense). The most straightforward use case is modulator buttons modulating device on/off controls or dry/wet mixes, or even those controls mapped to the page directly. A more elaborate example may be a Grid instrument with slew limiters, envelopes and latches, there is no limit to where your fantasy may take you.

## SuperScene

SuperScenes are arbitrary groups of clips, similar to Maschine. Up to 64 SuperScenes are available per project.

* **SONG** (short press) toggles SuperScene mode. Scene buttons are lit according to existing SuperScenes and rainbow colored.
* Empty **SCENE(1-8)** creates new SuperScene from playing clips
* Lit **SCENE(1-8)** launches clips in that SuperScene and stops others. Currently selected scene is **white**.
* **CLEAR+SCENE(1-8)** deletes the SuperScene (clips and their playing states are unaffected)
* **SHIFT+SCENE(1-8)** selects a SuperScene page (see **Page navigation**)

SuperScenes are saved with the project.

**NOTE**: SuperScene launcher can only operate on tracks that are currently visible in Bitwig.
Namely, if a clip is in a nested track and the group is folded, or not in the group when the group
is entered, SuperScene will not be able to launch or stop it directly. 

If a group track containing SuperScene clips in its inner tracks was folded, 
SuperScene will launch the _entire_ last (bottom-most) scene of that group track that has a playing clip.

## Device Selector

Note: there is currently a bug in the API that will cause Device Selector display to freak out when adding new devices. In the meantime, scroll the track bank back and forth to clear.

Allows directly selecting devices in **CONTROL** mode. Hold **CONTROL** to access this.

Keep **CONTROL** pressed for a little longer and Device Selector will become sticky - 
it will stay on after CONTROL button is released, unless you operate other controls while holding **CONTROL**. 
**CONTROL** button will flash when Device Selector is active.

If you don't need Device Selector you can turn it off with:

* **CONTROL+SELECT**: Toggle device matrix

In this mode the clip matrix shows devices in each track, just like in Mixer. Press a pad
to select a device (selecting a device also selects its track).

Devices are color coded:

* Native devices
  * Audio FX: orange
  * Instrument: yellow
  * Note FX: cyan
* Plugins
  * Audio FX: magenta
  * Instrument: lime
  * Note FX: plum

Disabled devices are lit more dimly than enabled. Currently selected device will flash.

Additionally:

* **CLEAR+PAD**: Delete device
* **DUPLICATE+PAD**: Copy device to another slot, same as with clips

### Toggling devices

When Device Selector is on, press **SELECT+PAD** to toggle a device. 
Note that Device Selector must be in sticky mode so that CONTROL button is not held, otherwise SELECT will behave differently.

### Page navigation

Use **ARROW** keys to:

* **LEFT/RIGHT**: scroll track bank page
* **UP/DOWN**: scroll device bank pages

**NOTE**: unlike scenes, device banks do not all scroll together. Instead, devices on each
track will scroll only if there are more devices to scroll to, others will stay in place.
That is, if you have one track with 20 devices and another with 1, you will always see 
the 1 device while the other 20 are scrolling.

## Track Selector

Hold **MACRO** to see track selector. Clip matrix displays 64 consecutive tracks, as they appear
in Bitwig (effectively, the matrix is 8 TRACK button rows for 8 pages of track bank). Press pad
to select a track, currently selected track is bright white.

Double-clicking a group track toggles expansion.

Holding **MACRO** also switches control strips to user control mode.

## User controls

A bank of 64 user controls available for general global mapping, controlled by the touch strips.

User controls are accessible in two ways:

* Momentarily, by holding **MACRO**
* In **CONTROL** mode, press **CONTROL+MACRO** to switch user controls on and off

The 64 controls are grouped in 8 pages. To select a page use the track buttons, currently selected page is brightly lit.

# Settings

## Global

* **Show pretty shift commands in matrix**: when enabled, holding **SHIFT**
will change the colors of the top row of the clip matrix buttons to indicate that they are special.
* **SHIFT-TRACK selects track page**: **SHIFT** turns track group row into page selectors, see **Page navigation**
* **DPAD scroll (regular/SHIFT)**: Flip **DPAD** function with and without **SHIFT**: arrows scroll by page/SHIFT-arrow by single row, or vice versa.
* **Limit level sliders**: slider range when controlling track levels
  * _None_: sliders behave as shown in the app (i.e., +6 dB is maximum)
  * _0 dB_: slider maximum is 0 dB for all tracks
  * _-10 dB_: slider maximum is -10 dB for all tracks
  * _Smart_: maximums are 0 dB for group tracks and -10 dB for regular tracks
* **SHIFT+PLAY**: Toggle between restart and pause/continue
* **Launch Q: Launch tolerance**: how late you can be for retroactive launch Q to work. 0.0 turns it off, 0.5-0.8 is probably a good range.
* **Verbose console output**: Enable if you're me or just really curious about internal workings of MonsterJam, otherwise leave it off.

After changing preferences it may be necessary to reinitialize the extension (turn it off an on again in Controllers settings, or select a different project).

## Project

(Open Studio I/O Panel and look under Maschine Jam)

* Hide disabled: tracks — disabled tracks are skipped

# Changelog

## 8.0

### New features

* Step sequencer (WIP)
* Lenient launch: launching clips on time or even a little late still applies correct quantization
* Natural track scrolling: instantly scroll by an arbitrary number of tracks (less than 8) by pressing a track button while holding another
* New PLAY button behavior
* Refactored Track Tracker no longer causes extraneous project modifications
* Setting to toggle console output
* API 17
* Completely refactored mode layers and button handlers


### Fixes

* Now compiles on Windows
* Fixed NPE while loading optional mappings on Windows
* Page matrix scrolling could result in errors
* SuperScenes did not work in newer Bitwig versions
* UserControl page navigation was broken
* Tap tempo (shift-tempo) would activate tempo mode
* Improved documentation
* Added stdout output for easier debugging
* Scala 3