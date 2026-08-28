#!/usr/bin/env bash

set -euo pipefail

sketchybar --set "${NAME}" label="$(date '+%a %d %b %H:%M')"
