# MonsterJam: Maschine JAM Bitwig Controller Extension

[![Download](https://img.shields.io/github/downloads/unthingable/monster-jam/total.svg)](https://github.com/unthingable/MonsterJam/releases/latest)
[![Github Release](https://img.shields.io/github/v/release/unthingable/monster-jam?include_prereleases)](https://img.shields.io/github/v/release/unthingable/MonsterJam?include_prereleases)

This is the source code repository for the MonsterJam Bitwig controller extension.

Precompiled releases can be found [here](https://github.com/unthingable/monster-jam/releases).

User documentation, including installation instructions, can be found [here](docs/README.md).

## Overview

This is intended as a replacement for Bitwig's bundled Maschine JAM extension. Requires Bitwig API 15+.

## Compiling

### Requirements

- [OpenJDK 12.x](https://adoptopenjdk.net/releases.html?variant=openjdk12)
- [Maven >= 3.1.0](https://maven.apache.org/)

### Build and install

1. Follow the installation instructions for each of the above requirements.
2. Run `mvn install`.

### Debugging

1. Set an environment variable `BITWIG_DEBUG_PORT` to an unused port number.
2. Restart Bitwig.
3. Setup your debugger to connect to the port from step 1.

## Contributing

Issues and requests welcome.
