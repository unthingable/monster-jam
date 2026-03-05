#!/usr/bin/env bash
# Generate target/build-info.properties for dev builds.
# Run by Maven via the dev-build-info profile (activated when this file exists).
# Not committed — bin/ is untracked.

set -euo pipefail

BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
if [ "$BRANCH" = "master" ]; then
  DUAL_PORTS=false
else
  DUAL_PORTS=true
fi

DESC=$(git describe --tags --match "devel-v*" 2>/dev/null || true)
if [ -z "$DESC" ]; then
  mkdir -p target
  printf 'buildSuffix=\ndualPorts=%s\n' "$DUAL_PORTS" > target/build-info.properties
  echo "[build-info] no devel tag found, buildSuffix empty"
  exit 0
fi

COUNTER_FILE="$(git rev-parse --git-common-dir)/monsterjam-build-number"
[ ! -f "$COUNTER_FILE" ] && echo "0" > "$COUNTER_FILE"

N=$(( $(cat "$COUNTER_FILE") + 1 ))
echo "$N" > "$COUNTER_FILE"

mkdir -p target
printf 'buildSuffix=-dev.%s\ndualPorts=%s\n' "$N" "$DUAL_PORTS" > target/build-info.properties
echo "[build-info] build #$N (dualPorts=$DUAL_PORTS)"
