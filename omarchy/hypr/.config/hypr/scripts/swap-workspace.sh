#!/bin/bash

CURRENT=$(hyprctl activeworkspace -j | jq '.id')
DEST=$(zenity --entry --title="Swap Workspace" --text="Swap workspace $CURRENT with:")

[[ ! "$DEST" =~ ^[0-9]+$ ]] && exit 1
[[ "$DEST" == "$CURRENT" ]] && exit 0

# Switch to destination first so waybar marks it active before any moves
hyprctl dispatch workspace "$DEST"

# Move dest → temp (99)
hyprctl clients -j | jq -r ".[] | select(.workspace.id == $DEST) | .address" | \
  xargs -I{} hyprctl dispatch movetoworkspacesilent "99,address:{}"

# Move current → dest
hyprctl clients -j | jq -r ".[] | select(.workspace.id == $CURRENT) | .address" | \
  xargs -I{} hyprctl dispatch movetoworkspacesilent "$DEST,address:{}"

# Move temp → current
hyprctl clients -j | jq -r ".[] | select(.workspace.id == 99) | .address" | \
  xargs -I{} hyprctl dispatch movetoworkspacesilent "$CURRENT,address:{}"
