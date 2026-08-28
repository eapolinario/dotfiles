#!/usr/bin/env bash

set -euo pipefail

# The space items are built once, when sketchybar loads its config. Plugging in
# or unplugging a display adds or removes mission control spaces, which would
# otherwise leave the indicator showing a stale set. Reload when the spaces
# yabai reports no longer match the ones currently on the bar.

current="$(yabai -m query --spaces 2>/dev/null | jq -r '[.[].index] | join(",")' 2>/dev/null || true)"
if [[ -z $current ]]; then
	exit 0
fi

rendered="$(sketchybar --query bar |
	jq -r '[.items[] | select(startswith("space.")) | ltrimstr("space.")] | join(",")')"

if [[ $current != "$rendered" ]]; then
	sketchybar --reload
fi
