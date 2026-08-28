#!/usr/bin/env bash

set -euo pipefail

# sketchybar passes the new front application name in INFO.
if [[ ${SENDER:-} == "front_app_switched" ]]; then
	sketchybar --set "${NAME}" label="${INFO}"
fi
