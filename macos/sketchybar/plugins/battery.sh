#!/usr/bin/env bash

set -euo pipefail

CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
readonly CONFIG_DIR

# shellcheck source=../colors.sh
source "${CONFIG_DIR}/colors.sh"

battery_info="$(pmset -g batt)"
percentage="$(
	awk 'NR == 2 && match($0, /[0-9]+%/) {
		print substr($0, RSTART, RLENGTH)
	}' <<<"$battery_info"
)"
state="$(awk -F'; *' 'NR == 2 { print $2 }' <<<"$battery_info")"

if [[ -z $percentage ]]; then
	printf 'Unable to read battery percentage\n' >&2
	exit 1
fi

numeric_percentage="${percentage%\%}"
icon="BAT"
color="$LABEL_COLOR"

case "$state" in
charging | charged | "finishing charge")
	icon="BAT+"
	color="$ACCENT_COLOR"
	;;
esac

if ((numeric_percentage <= 20)); then
	color="$WARNING_COLOR"
fi

sketchybar --set "$NAME" \
	icon="$icon" \
	label="$percentage" \
	label.color="$color"
