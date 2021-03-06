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

* **CLEAR**: Use in combination with other buttons to delete a scene (scene buttons), clip (a pad in session mode) or track (group buttons).
* **DUPLICATE**: Combine with a scene pad (duplicate scene) or a track button (duplicate track). To copy clips in session mode keep the Duplicate button pressed; choose the source clip (it must be a clip with content, you can still select a different clip as the source); select the destination clip (this must be an empty clip, which can also be on a different track); release the Duplicate button.
* **SHIFT+DUPLICATE (DOUBLE)**: Double the content of the currently selected clip (not the clip itself).
* **KNOB turn**: Move arranger playhead, jog through the project timeline. Hold **SHIFT** to adjust tempo in finer increments.
* **SHIFT-PAD (top row)**: The buttons in the top row of clip matrix change to their alternate functions, as labeled.
* **SHIFT-PAD (second row)**: Additional functions and settings
  * **SHIFT-PAD 1**: Toggle hiding/showing disabled tracks
* **SONG**: **SuperScene** mode (see below)
* **MACRO**: Activate **Track Selector** and **user controls** mode (see **CONTROL**)
* **NOTE REPEAT**: Fill mode

## Transport

* **PLAY**: Start/Stop playback
* **REC**: Toggle recording
* **SHIFT+PLAY** (RESTART): Rewind play position (stop if playing)
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

## Page navigation

* The arrow keys scroll the grid by blocks of 8 tracks/scenes. Keys will light up if there is content to scroll to.
* **SHIFT+...** in regular mode: hold shift and then
  * Arrow keys: scroll by 1 track/scene (SHIFT function is flippable in preferences)
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
  * Note: maximum slider range can be limited to values other than +6 dB, see **Preferences**.
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
are self-gating: if you press and hold a track button, operate any touchstrip or just wait long enough, it will return
to the previously selected parameter when track button is released. This is useful for momentary adjustments.

To select a device and a page: whichever device was selected last on a track will be the device controlled in Slice mode.
Same goes for remote control pages within a device. You can use Device Selector to quickly select a device on each track.

# Modes and notes

## Self-gating modes

A quick press on a mode button turns it on, but hold the button and 
make mode edits (or just wait long enough) and it returns to the previous mode when released - very handy for quick navigation
and performance.

Some modes that are like this:
* Solo, Mute, and TEMPO
* User control pages
* Control slice selectors

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

Usage example:
* Make a preset page of buttons called `MonsterFX` in a container to enable/disable FX devices, or do anything else.

It's a bit of a hack until a better way is found.

## SuperScene

SuperScenes are arbitrary groups of clips, similar to Maschine. Up to 64 SuperScenes are available per project.

* **SONG** toggles SuperScene mode. Scene buttons are lit according to existing SuperScenes and rainbow colored.
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

# Preferences

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
* **Enable track tracker**: use extreme cleverness to follow the tracks as layout changes.
  * Why you want this:
    * SuperScenes will work correctly even if tracks have changed positions
    * Scroll window will maintain its position when folding/unfolding tracks
  * Why you may not want this: track tracker introduces tiny variations in track colors (specifically the alpha channel).
    * This modifies the project even if you didn't do anything, so it will ask you to save
    * Track color selectors will not display the color as selected

After changing preferences it may be necessary to reinitialize the extension (turn it off an on again in Controllers settings, or select a different project).

## Project

(Open Studio I/O Panel and look under Maschine Jam)

* Hide disabled: tracks ??? disabled tracks are skipped
