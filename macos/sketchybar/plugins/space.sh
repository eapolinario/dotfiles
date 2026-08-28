#!/usr/bin/env bash

set -euo pipefail

# Driven by the sketchybar `space` component. sketchybar injects NAME, SID and
# SELECTED; SELECTED flips to true/false as the associated mission control
# space gains or resigns focus. The component only re-runs this on an actual
# change in SELECTED, so there is no polling.

case "${SENDER:-}" in
mouse.clicked)
	# Focusing a space works with SIP enabled. Creating or destroying one needs
	# the scripting addition, which is why the bar never offers that.
	yabai -m space --focus "${SID}" 2>/dev/null || true
	;;
*)
	sketchybar --set "${NAME}" \
		icon.highlight="${SELECTED}" \
		background.drawing="${SELECTED}"
	;;
esac
