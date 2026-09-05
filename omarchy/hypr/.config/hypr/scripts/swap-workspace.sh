#!/usr/bin/env bash

# Swap the windows of the current workspace with those of another one.
#
# Hyprland 0.56 evaluates `hyprctl dispatch` arguments as Lua, so the classic
# `hyprctl dispatch workspace 3` form fails with a parse error and the action
# never lands. Every dispatch below uses the Lua API instead.

set -euo pipefail

fail() {
  printf 'Swap workspace: %s\n' "$1" >&2
  exit 1
}

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    fail "Missing dependency: $1"
  fi
}

focus_workspace() {
  if ! hyprctl dispatch "hl.dsp.focus({ workspace = \"$1\" })" >/dev/null; then
    fail "Unable to focus workspace $1."
  fi
}

move_window() {
  if ! hyprctl dispatch \
    "hl.dsp.window.move({ workspace = \"$1\", follow = false, window = \"address:$2\" })" >/dev/null; then
    fail "Unable to move window $2 to workspace $1."
  fi
}

main() {
  require_cmd hyprctl
  require_cmd jq
  require_cmd zenity

  local active current dest status clients moves workspace address
  if ! active=$(hyprctl activeworkspace -j); then
    fail 'Unable to query the active workspace.'
  fi
  if ! current=$(jq -ers '
    if length != 1 then error("expected one active workspace") else .[0].id end
    | select(type == "number" and . == floor and . > 0 and . <= 2147483647)
  ' <<<"$active"); then
    fail 'Unable to parse a positive active workspace ID.'
  fi

  if dest=$(zenity --entry --title="Swap Workspace" --text="Swap workspace $current with:"); then
    # Strip leading zeroes before numeric comparisons, avoiding Bash's octal syntax.
    [[ "$dest" =~ ^0*([1-9][0-9]*)$ ]] || fail 'Destination must be a positive workspace ID.'
    dest="${BASH_REMATCH[1]}"
    if ((${#dest} > 10)) || ((dest > 2147483647)); then
      fail 'Destination workspace ID must not exceed 2147483647.'
    fi
  else
    status=$?
    ((status == 1)) && return 0
    fail "Unable to prompt for a workspace (zenity exited $status)."
  fi
  [[ "$dest" != "$current" ]] || return 0

  if ! clients=$(hyprctl clients -j); then
    fail 'Unable to query workspace windows.'
  fi
  # Plan both original window sets from one snapshot, before changing any workspace.
  if ! moves=$(jq -rs --argjson current "$current" --argjson dest "$dest" '
    if length != 1 or (.[0] | type) != "array"
    then error("expected one clients array")
    else .[0] end
    | .[]
    | select(.workspace.id == $current or .workspace.id == $dest)
    | if (.address | if type == "string" then test("^0x[0-9a-fA-F]+$") else false end)
      then [if .workspace.id == $current then $dest else $current end, .address] | @tsv
      else error("invalid client address")
      end
  ' <<<"$clients"); then
    fail 'Unable to parse workspace windows.'
  fi

  # Switch first so the bar marks the destination active before any moves.
  focus_workspace "$dest"

  while IFS=$'\t' read -r workspace address; do
    [[ -n "$address" ]] || continue
    move_window "$workspace" "$address"
  done <<<"$moves"
}

main "$@"
