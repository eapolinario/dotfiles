#!/usr/bin/env bash

set -euo pipefail

CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
readonly CONFIG_DIR
readonly STATUS_SCRIPT="${CONFIG_DIR}/../tmux/status_cpu_mem.sh"

if [[ ! -x $STATUS_SCRIPT ]]; then
	printf 'Missing resource monitor: %s\n' "$STATUS_SCRIPT" >&2
	exit 1
fi

sketchybar --set "$NAME" label="$("$STATUS_SCRIPT")"
