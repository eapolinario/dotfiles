#!/usr/bin/env bash
# Move focus in given direction and warp cursor to center of new active window
hyprctl dispatch movefocus "$1"
read -r cx cy < <(hyprctl -j activewindow | jq -r '[.at[0] + (.size[0] / 2 | floor), .at[1] + (.size[1] / 2 | floor)] | @tsv')
hyprctl dispatch movecursor "$cx" "$cy"
