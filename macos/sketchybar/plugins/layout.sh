#!/usr/bin/env bash

set -euo pipefail

layout="$(
	aerospace list-workspaces --focused \
		--format "%{workspace-root-container-layout}"
)"

case "$layout" in
h_tiles)
	icon="TILE"
	label="H"
	;;
v_tiles)
	icon="TILE"
	label="V"
	;;
h_accordion)
	icon="STACK"
	label="H"
	;;
v_accordion)
	icon="STACK"
	label="V"
	;;
*)
	icon="LAYOUT"
	label="?"
	;;
esac

sketchybar --set "$NAME" icon="$icon" label="$label"
