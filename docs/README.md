# MonsterJam

Support for Native Instruments Maschine JAM. 

This only works on Mac and Windows.

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

**NOTE**: Key bindings are experimental and may change in future versions.

## Global

* **CLEAR**: Use in combination with other buttons to delete a scene (scene buttons), clip (a pad in session mode) or track (group buttons).
* **DUPLICATE**: Combine with a scene pad (duplicate scene) or a group button (duplicate track). To copy clips in session mode keep the Duplicate button pressed; choose the source clip (it must be a clip with content, you can still select a different clip as the source); select the destination clip (this must be an empty clip, which can also be on a different track); release the Duplicate button.
* **SHIFT+DUPLICATE (DOUBLE)**: Double the content of the currently selected clip (not the clip itself).
* **KNOB turn**: Jog through the project timeline
* **SHIFT-PAD (top row)**: The buttons in the top row of clip matrix change to their alternate functions, as labeled.
* **SONG**: SuperScene mode

## Transport

* **PLAY**: Start/Stop playback
* **SHIFT+PLAY** (RESTART): Rewind play position (stop if playing)
* **SHIFT+RECORD**: Toggle launcher overdub
* **SHIFT+PAGE LEFT**: Toggle the metronome
* **SHIFT+PAGE RIGHT**: Toggle transport loop
* **SHIFT+TEMPO**: Tap Tempo
* **TEMPO+KNOB turn**: change tempo
* **GRID**: Hold to change launch grid quantization with SCENE buttons (see Launch grid quantization below)
* **SOLO**: Solo mode, press track(group) buttons to enable/disable solo. Keep holding SOLO button to automatically 
  disable Solo mode when released.
* **MUTE**: Mute mode, same as Solo

## Clip Launcher

* **(PAD)** on empty clip: record new clip
* **(PAD)** on existing clip: start clip playback
* **(PAD)** on playing clip: stop clip playback
* **SELECT+(PAD)**: Select the clip without starting it. Alternatively, *long press* the pad.
  While **SELECT** is pressed the currently selected clip is WHITE.
* **CLEAR+(PAD)**: Delete the clip
* **DUPLICATE**: To duplicate a clip keep the duplicate button pressed; choose the source clip (it must be a clip with content, you can still select a different clip with content); select the destination clip (this must be an empty clip, which can also be on a different track); release the Duplicate button.

### Page navigation

* The arrow keys scroll the grid by blocks of 8 tracks/scenes. Keys will light up if there is content to scroll to.
* **SHIFT+** in regular mode: hold shift and then
  * Arrow keys: scroll by 1 track/scene
  * **SCENE(1-8)**: directly select from the first 8 pages of scenes
  * **GROUP(1-8)**: directly select from the first 8 pages of tracks (if enabled in settings)
  * The currently selected scene/track page is **white**, available pages are **yellow**. If the view is currently between pages, two adjacent pages will be orange.
* **SHIFT+** in SuperScene mode:
  * Group buttons and arrow keys same as above
  * **SCENE(1-8)**: select SuperScene page. Pages with contents are **cyan**, current page is **white**, page with last selected scene is **lime**.
  * **(PAD) (bottom row)**: scene page selector is preserved but is now on the bottom matrix row instead of SCENE buttons.

## Group Buttons (A-H) and Scene Buttons (1-8)

The **group buttons (A-H)** select the tracks in the current page of the track bank.

The **scene buttons (1-8)** launch the scenes in the current page of the scene bank. Track buttons will light up on MIDI events 
from the track, for additional fun and profit (like Maschine).

## Touch Strips

* **LEVEL**: Toggles between Volume and Panorama editing of 8 tracks. If Volume is active and playback is started the VU of the tracks is displayed as well. All strips are lit in their tracks' color.
* **AUX**: Edit send levels for a specific send for all tracks. Strips are lit in corresponding Effect track colors.
* **AUX+GROUP(A-H)**: Hold **AUX** down to select from the first 8 Effect tracks. Group buttons are lit in corresponding 
  Effect track colors, currently selected send is in WHITE.
* **CONTROL**: Edit device remote controls
  * **PAGE LEFT/PAGE RIGHT**: Select previous/next device in the device chain
  * **CONTROL + PAGE LEFT/PAGE RIGHT**: Select previous/next parameter page in the current device. Page buttons light up 
    when there are pages to navigate to.

### Fine adjustment

Hold **SHIFT** to reduce strip sensitivity and adjust in finer increments. Strips behave like relative controls. This works in all touchstrip modes.

# Modes and notes

## Self-gating modes

This is something Novation Circuit does well: a quick press on a mode button turns it on, but hold the button and 
make mode edits (or just wait long enough) and it returns to the previous mode when released - very handy for quick navigation
and performance.

MonsterJam does the same with two modes currently, Solo and Mute, more will be added as needed.

## Launch grid quantization

Hold **GRID** to select the current launch quantization with **SCENE** buttons. Buttons correspond to the following values:

* "8", "4", "2", "1", "1/2", "1/4", "1/8", "1/16"

THe current setting is lit. Press the lit button to disable grid quantization altogether.

## PERFORM FX

Maschine native has a great PERFORM mode, where dual-touch strips control both an effect parameter and effect routing
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

# Preferences

* _Show pretty shift commands in matrix_: when enabled, holding **SHIFT**
will change the colors of the top row of the clip matrix buttons to indicate that they are special.
* _SHIFT-GROUP selects track page_: see **Page navigation**

After changing preferences it may be necessary to reload the extension (turn it off an on again in Controllers settings page).
Also, after upgrading MonsterJam version be sure to check the settings - deprecated settings 
may not get automatically removed by Bitwig, in which case delete and add the controller again.