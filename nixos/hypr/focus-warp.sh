#!/usr/bin/env bash
# Move focus in given direction and warp cursor to center of new active window.
# Uses ydotool (uinput-level) instead of hyprctl movecursor because VMware sends
# absolute mouse coordinates, which would override a compositor-level warp on
# the next mouse movement.
hyprctl dispatch movefocus "$1"
read -r cx cy < <(hyprctl -j activewindow | jq -r '[.at[0] + (.size[0] / 2 | floor), .at[1] + (.size[1] / 2 | floor)] | @tsv')
ydotool mousemove --absolute -x "$cx" -y "$cy"
