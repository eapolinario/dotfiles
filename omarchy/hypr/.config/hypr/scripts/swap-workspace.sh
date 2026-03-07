#!/bin/bash

CURRENT=$(hyprctl activeworkspace -j | jq '.id')
DEST=$(zenity --entry --title="Swap Workspace" --text="Swap workspace $CURRENT with:")

[[ ! "$DEST" =~ ^[0-9]+$ ]] && exit 1
[[ "$DEST" == "$CURRENT" ]] && exit 0

# Move current → temp (99)
hyprctl clients -j | jq -r ".[] | select(.workspace.id == $CURRENT) | .address" | \
  xargs -I{} hyprctl dispatch movetoworkspacesilent "99,address:{}"

# Move dest → current
hyprctl clients -j | jq -r ".[] | select(.workspace.id == $DEST) | .address" | \
  xargs -I{} hyprctl dispatch movetoworkspacesilent "$CURRENT,address:{}"

# Move temp → dest
hyprctl clients -j | jq -r ".[] | select(.workspace.id == 99) | .address" | \
  xargs -I{} hyprctl dispatch movetoworkspacesilent "$DEST,address:{}"

hyprctl dispatch workspace "$DEST"
