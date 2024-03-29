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

* **KNOB turn**: Move arranger playhead, jog through the project timeline. Hold **SHIFT** to adjust in finer increments (e.g. in TEMPO mode).
* **SONG**: **SuperScene** mode (see below) or "home" (return to default Clip Launcher view if not already there)
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
* **REC (hold)**: Arm tracks via track buttons
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
* **STEP+CLEAR**: Clear current clip
* **STEP+DUPLICATE**: Duplicate content of current clip
* **STEP+PLAY**: Toggle pattern follow
* **STEP (hold)**: Display quick clip selector

Step sequencer settings are stored per track and saved with the project. Most steq sequencer submodes are auto-gating.

Whenever a sequencer state changes, a relevant notification will pop up.

## Default layout

* **SCENE** (top): pattern pages. Currently selected page is bright white, currently playing page is dim white. Hold **SHIFT** to select between banks of 8 pages.
* **PAD** matrix: select and edit steps. When transport is playing the currently playing step is white (a chasing light).
* **DPAD** arrows up/down: scroll notes up or down by pages, left/right: by one step
* **KNOB**: scroll notes up or down by one
* **KNOB (push turn)**: scroll steps left/right
* **GRID**: activate grid selector
* **SHIFT+SOLO**: activate pattern length selector
* **NOTES**: activate Note/Velocity mode
* **PERFORM (hold)**: change current MIDI channel, scale

The button grid is bottom-focused. Note mode will push the grid up. When using note pages, the first note in the page will be the bottom-most visible note on the grid.

### Quick clip selector

Hold **STEP** to see the clip matrix. Currently selected clip will be bright WHITE. Press any other clip to focus the step sequencer on it. If a slot is empty, new clip will be created. If transport is playing, newly created clips will play automatically.

### Scene buttons 

Display/select current pattern page.

The number of pattern pages depends on both the step size and the current grid layout, there will be as many pages as necessary to access the entire length of the pattern (though only the first 8 are directly accessible).

Scene button color guide:
* Playing: blinking white
* Curently selected: bright white
* Non-empty: dim white
* Empty: clip color

### Pad matrix

* Press empty step (dark button) to create a step
* Press an existing step to clear (and release quickly)
* Press and hold step to select it for editing. Releasing after a long press will not delete it.
* **SHIFT+NOTES** to toggle alternating note row colors

If a note spans more than one step, consecutive steps are lit in dim white.

#### Entering legato notes

Press and hold a pad to either select or create a step. Click another pad on the right to change note length in steps.

Extending a note all the way to the next step makes it legato.

## Grid mode and pattern pages

The step sequencer matrix is a windowed view of a clip, showing up to 64 steps at once.

*Step size* determines how many steps are in a clip (depends on clip length).

* **GRID**: activate grid selector to change grid configuration
* **SCENE**: set how many rows are given to each note
  * 1: 1 note per 8 rows
  * 2: 2 notes per 8 rows (4 rows per note)
  * 3: 4 notes per 8 rows (2 rows per note)
  * 4: 8 notes per 8 rows (1 row per note)
* **KNOB**: change step size

## Note interlace

With nontrivial grid sizes (i.e. not 8x8 or 1x64) you can choose between different wrapping modes and pick what works for you. The toggle is in controller settings.

For example, if Grid is 4x16 (showing 4 notes, 16 steps each), the different modes might look as follows, with `-` being the currently playing step:

Interlaced:
```
11-11111
11111111
22-22222
22222222
33-33333
33333333
44-44444
44444444
```
Non-interlaced (whole pattern wrap):
```
11-11111
22-22222
33-33333
44-44444
11111111
22222222
33333333
44444444
```

## Note/Velocity submode

* **NOTES**: activate note/velocity selectors
* **NOTES (hold)**: display note page selectors (scene buttons)
* **NOTES+scene**: select a note page (each page is 16 notes)

When NOTES is on, the lower half of the pad matrix changes function and is solidly lit with the color of the clip. The mode itself is split in two:
* Left half: velocity selector
* Right half (numbered pads): note selector

NOTES mode is per track.

### Velocity selector

Coarse velocity adjustment in 16 steps (1 step = 8 velocity steps, out of 128). Currently selected velocity step is white.

Velocity selector works with two different velocity values:
* Default velocity: newly entered note steps will use this
* Velocity of a specific note step: hold one or more note steps, then you can see/edit its velocity via the selector. If multiple steps are held, it's the last pressed step that takes precedence, and adjustments are made to all of them simultaneously.

### Note selector

Play notes and focus step editor.

The lower right quadrant (numbered pads) correspond to 16 consecutive notes (in scale), starting with the bottom of the current note page (see below). The "bank" of white buttons are notes currently visible in the step grid.

Pressing a pad:
* Plays the note
* Scrolls the step editor to that note "bank" (hold SELECT to ignore banking and scroll to that note directly, or use encoder)

The note selector button corresponding to the bottommost visible note is bright white. If more notes are visible in the step view, their buttons will be dim white.

Currently playing non-white notes will flash, letting you see activity outside of the visible step grid.

#### Note pages

**Press and hold NOTES** to see the page selector (SCENE buttons), higher notes are on the right like on a piano. Unlike Note selector that is fixed to a page, page selector will indicate if current note window straddles two pages, pages with visible content will be in dim white. Pressing a button always scrolls bottom note row to the bottom of that page. If the grid is aligned exactly to the beginning of a page, its page button will be bright white.

Seeing how the Drum Machine lays out its banks, in chromatic scale the first page is 4 notes while the rest are 16. This is a compromise where the page layout matches Drum Machine's (but you cannot access the last page with Scene buttons because there are only 8 — can still scroll though). In non-chromatic mode all pages are 16 notes.

Use note/page selectors together with knob scrolling for maximum nagivation.

Note: note page selector always displays pages for the full 8x8 grid, regardless of whether NOTES mode is active.

## Channel and Scale

Hold **PERFORM** to access channel and scale selectors.

### Channel selector

16 buttons in bottom right quadrant change active channel for clip notes. Currently selected channel is white.

### Scale selector (top 4 rows)

Works more or less exactly like Novation Circuit. Unlike the Circuit, changing a scale will not re-transpose existing notes, notes not in current scale will simply not be visible.

* Top 2 rows: select root note, current note is dark blue
* Next 2 rows: select scale, current is bright pink

The 16 scale buttons are laid out as follows, Chromatic scale is the default. Note that non-chromatic scales all have exactly 8 notes per octave.
1. Natural Minor            
1. Major                    
1. Dorian                   
1. Phrygian                 
1. Mixolydian               
1. Melodic Minor (ascending)
1. Harmonic Minor           
1. Bebop Dorian             
1. Blues                    
1. Minor Pentatonic         
1. Hungarian Minor          
1. Ukranian Dorian          
1. Marva                    
1. Todi                     
1. Whole Tone               
1. Chromatic                

## Parameter/expressions adjustments (WIP)

Press and hold existing step(s) to access note step parameters via sliders. When multiple steps are held, last pressed step's value is used as a starting point but all are adjusted together. Make fine adjustments by holding SHIFT.

8 parameters are available for adjustment (more TBA later):

1. Note velocity
1. Note release velocity
1. Note velocity spread
1. Note start offset, within one step, in 128 increments
1. Duration, within a step (if a note spans mutliple steps, this is duration of the last occupied step)
1. Pan
1. Timbre
1. Pressure

Parameter adjustment mode has two submodes, controlled via track/group buttons (above sliders). Submode selections are stored per track in the project.

It is useful to have the clip editor open, especially when working with nudge and duration, so see how changes are affecting the notes.

### Note mode (default)

In this mode each of the 8 parameters is controlled by a corresponding slider (1: velocity, 3: spread, etc.). When holding multiple steps, the value of the last  pressed step will be shown on the LED strip. Moving a slider applies the parameter change to all held steps (absolute, not relative).

### Row mode

Press any of the track buttons to activate single row mode. The buttons correspond to each of the 8 parameters, the currently selected parameter will be **WHITE**. Press it again to deactivate and return to Note mode (above).

In Row mode each slider controls one specific parameter for the corresponding step on that row (if it has a step) — exactly like the expressions view in Bitwig clip editor.

Example: say you have 4 active steps (1,1,1,1,0,0,0,0) on a given row and you want to quickly adjust velocities:
1. Hold down any one of the active steps
1. If track/group A (single-row velocity) if not selected already, press it
1. First 4 sliders will show current velocities, second 4 will be blank
1. Move sliders to change note velocities

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
* SOLO, MUTE, TEMPO, RECORD
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

**NOTE**: SuperScenes are experimental and are built using undocumented implementation details of Bitwig API to derive unique track IDs. There is a chance things will randomly stop working with a new release.

### ID collision warnings

Due to experimental nature of the underlying track ID mechanism, MonsterJam monitors track IDs for uniqueness. If a duplicate ID is detected, a notification will pop up with the names of tracks.

If this happens, try duplicating the offending track and deleting the original, and definitely let me know.

## Device Selector

Allows directly selecting devices in **CONTROL** mode. Hold **CONTROL** to access this.

* **PAD**: select device
* **SCENE(1-8)**: select track remotes
* **MST**: select project remotes

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

* **Display**
    * **Show pretty shift commands in matrix**: when enabled, holding **SHIFT**
will change the colors of the top row of the clip matrix buttons to indicate that they are special (like in DbM)
* **Step Sequencer**
  * **Note interlace**: control how long note rows are wrapped, depending on Grid settings
    * On: rows grouped by note (each note row wrapped individually)
    * Off: single row per note (the whole pattern is wrapped)
  * **Alternating row colors**: in step sequencer, each note row gets a different color
  * **Step sequencer pattern follow**: active step sequencer pattern page will automatically follow playing position
  * **Step sequencer note preview**: pressing steps in sequencer will play corresponding notes
  * **Keep note end**: when nudging steps right, automatically reduce note duration keep the note from extending into next empty step
* **Behavior**
  * **SHIFT-TRACK selects track page**: **SHIFT** turns track group row into page selectors, see **Page navigation**
  * **DPAD scroll (regular/SHIFT)**: Flip **DPAD** function with and without **SHIFT**: arrows scroll by page/SHIFT-arrow by single row, or vice versa.
  * **Limit level sliders**: slider range when controlling track levels
    * _None_: sliders behave as shown in the app (i.e., +6 dB is maximum)
    * _0 dB_: slider maximum is 0 dB for all tracks
    * _-10 dB_: slider maximum is -10 dB for all tracks
    * _Smart_: maximums are 0 dB for group tracks and -10 dB for regular tracks
  * **SHIFT+PLAY**: Toggle between restart and pause/continue
* **Launcher**
  * **Launch Q forgiveness**: how late you can be for retroactive launch Q to work. 0.0 turns it off, 0.5-0.8 is probably a good range.
  * **Launch Q lookahead**: compensation for event processing delay. If you attempt to launch on-beat but still miss the window, increase this value.
* **Debug**
  * **Verbose console output**: Enable if you're me or just really curious about internal workings of MonsterJam, otherwise leave it off.

After changing preferences it may be necessary to reinitialize the extension (turn it off an on again in Controllers settings, or select a different project).

## Project

(Open Studio I/O Panel and look under Maschine Jam)

* Hide disabled: tracks — disabled tracks are skipped

# Changelog

## 8.0b18

* Added support for non-classic color schemes
* API 18, Scala 3.3.1, Java 17

## 8.0b17

Step sequencer fixes and improvements

* Added small delay between step press and note param activation (sliders) for less flashy sequence editing. Delay is equal to when the step press becomes long press.
* Added a setting to keep notes within their original steps when nudging
* In "Quick Clip Selector" (hold STEP), pressing an empty slot will create a new clip and select it
* When transport is playing, newly created clips will play too
* Added "note interlace" setting
* Changed alternating note colors to 8 rainbow colors, for better visual separation
* Refactored controller settings: re-add MonsterJam in your Bitwig 

## 8.0b16

Step sequencer fixes and improvements

* Updated color scheme for alternarting note rows: now it's the note itself that gets the color instead of the background, looks much better
* Fixed regression in quick clip selector activation, could break switching between STEP mode and clip matrix
* Fixed regression in pattern page follow while holding steps, condition was flipped
* Fixed quick clip selector colors, was showing empty scene clips as white
* Fixed SHIFT button forgetting its normal bindings in step mode

Other fixes

* Fixed SONG button indicator for superscene mode, was not lighting up
* REC+GROUP to arm multiple tracks won't trigger transport record unless you mean it (with a single short press of REC)

## 8.0b15

* Note expressions editor
  * Now self-activated when steps are held, no longer need to press TUNE
  * Added single row mode, control one parameter for all notes in a row
  * Nudging a note will shorten it if it bumps into the next note, instead of bumping the next note
  * Duration adjustments are limited to a single step (so it won't clobber the next note)
  * Added parameter popup notifications
* Step sequencer
  * Added pattern page bank selectors for accessing up to 64 pages (via SHIFT-SCENE)
  * Added quick clip selector (hold STEP)
  * Pattern page follow disabled when holding steps
  * Previous slider mode auto-restores after expressions editor

* Fixed: step size and pattern scroll position could be set impromperly when switching tracks
* Fixed: currently playing pages no longer blink for non-playing clips

## 8.0b14
* Step: multi-row notes were wrapping incorrectly
* Step: NOTES - improved activation (experimental). Now a single press activates mode, but holding+SCENE navigates to note pages without activating

## 8.0b13
### Features
* Step: note preview + setting. Pressing any step in step sequencer will play that note.
* Step: NOTES mode now restores automatically and reliably, per track

### Fixes
* All scales are now correct, not just C
* It was possible for sequencer state to save on the wrong track, now much more robust

## 8.0b12
(changelog since b9)
### Features
* Step: note selector jumps in banks
* Hold REC to arm multiple tracks
* Clear REC/MUTE/SOLO on all tracks with CLEAR
* Clip queued for recording will flash red
### Fixes
* No more collision warnings that TrackTracker complained about
* Adding new step would display wrong velocity in NOTES mode
* Step sequencer was mapped to wrong notes and not aligned with note pads

### Known issues
* NOTES mode in sequencer does not reactivate automatically
* After using TUNE, previous strip mode will not reactivate

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