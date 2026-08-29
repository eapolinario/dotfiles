#!/usr/bin/env bash

set -euo pipefail

readonly WORKSPACE="$1"

if [[ $WORKSPACE == "${FOCUSED_WORKSPACE:-}" ]]; then
	sketchybar --set "$NAME" \
		icon.highlight=on \
		background.drawing=on
else
	sketchybar --set "$NAME" \
		icon.highlight=off \
		background.drawing=off
fi
