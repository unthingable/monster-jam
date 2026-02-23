#!/usr/bin/env bash
# Run integration tests. Usage:
#   tests/run.sh              # Tier 1 only (any project state)
#   tests/run.sh --all        # All tiers (needs test project)
#   tests/run.sh [pytest args]  # Pass-through, e.g. tests/run.sh -k play -v

set -euo pipefail
cd "$(dirname "$0")/integration"

args=("$@")
if [[ ${#args[@]} -eq 0 ]]; then
    args=(-m "not known_project" -v)
elif [[ "${args[0]}" == "--all" ]]; then
    shift
    args=(-v "$@")
fi

exec uv run --with pytest python -m pytest "${args[@]}"
