#!/usr/bin/env bash

# Swap the windows of the current workspace with those of another one.
#
# Hyprland 0.56 evaluates `hyprctl dispatch` arguments as Lua, so the classic
# `hyprctl dispatch workspace 3` form fails with a parse error and the action
# never lands. Every dispatch below uses the Lua API instead.

set -euo pipefail

readonly SCRATCH_WORKSPACE=99

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    printf 'Missing dependency: %s\n' "$1" >&2
    exit 1
  fi
}

focus_workspace() {
  hyprctl dispatch "hl.dsp.focus({ workspace = \"$1\" })" >/dev/null
}

# Move every window on $1 to $2 without following them.
move_workspace_windows() {
  local from="$1" to="$2" address

  while read -r address; do
    [[ -n "$address" ]] || continue
    hyprctl dispatch \
      "hl.dsp.window.move({ workspace = \"$to\", follow = false, window = \"address:$address\" })" >/dev/null
  done < <(hyprctl clients -j | jq -r --argjson from "$from" '.[] | select(.workspace.id == $from) | .address')
}

main() {
  require_cmd hyprctl
  require_cmd jq
  require_cmd zenity

  local current dest
  current=$(hyprctl activeworkspace -j | jq -r '.id')
  dest=$(zenity --entry --title="Swap Workspace" --text="Swap workspace $current with:")

  [[ "$dest" =~ ^[0-9]+$ ]] || exit 1
  [[ "$dest" != "$current" ]] || exit 0

  if [[ "$dest" == "$SCRATCH_WORKSPACE" || "$current" == "$SCRATCH_WORKSPACE" ]]; then
    printf 'Workspace %s is used as scratch space for the swap.\n' "$SCRATCH_WORKSPACE" >&2
    exit 1
  fi

  # Switch first so the bar marks the destination active before any moves.
  focus_workspace "$dest"

  move_workspace_windows "$dest" "$SCRATCH_WORKSPACE"
  move_workspace_windows "$current" "$dest"
  move_workspace_windows "$SCRATCH_WORKSPACE" "$current"
}

main "$@"
