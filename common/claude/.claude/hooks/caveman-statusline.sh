#!/bin/bash
# Stable wrapper — delegates to latest caveman plugin version in cache.
# Settings.json points here; plugin updates don't break the path.

SCRIPT=$(ls -1d "${CLAUDE_CONFIG_DIR:-$HOME/.claude}/plugins/cache/caveman/caveman/"*/hooks/caveman-statusline.sh 2>/dev/null | tail -1)
[ -z "$SCRIPT" ] && exit 0
[ ! -f "$SCRIPT" ] && exit 0
exec bash "$SCRIPT"
