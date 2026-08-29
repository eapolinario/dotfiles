#!/usr/bin/env bash

set -euo pipefail

readonly WORKSPACE="$1"

window_count="$(aerospace list-windows --workspace "$WORKSPACE" --count)"
focused_workspace="$(aerospace list-workspaces --focused)"

if ((window_count == 0)); then
	sketchybar --set "$NAME" drawing=off
elif [[ $WORKSPACE == "$focused_workspace" ]]; then
	sketchybar --set "$NAME" \
		drawing=on \
		icon.highlight=on \
		background.drawing=on
else
	sketchybar --set "$NAME" \
		drawing=on \
		icon.highlight=off \
		background.drawing=off
fi
