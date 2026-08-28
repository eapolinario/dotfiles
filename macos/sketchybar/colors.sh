#!/usr/bin/env bash
# Catppuccin Mocha palette, sourced by sketchybarrc and the plugins.
# Kept in sync with the border colors set in macos/yabai/yabairc so the bar and
# the active-window border read as one theme.
#
# This file only defines constants for its callers, so every variable is unused
# from shellcheck's point of view when the file is linted on its own.
# shellcheck disable=SC2034

readonly BAR_COLOR=0xff1e1e2e     # base
readonly ITEM_BG_COLOR=0xff313244 # surface0
readonly LABEL_COLOR=0xffcdd6f4   # text
readonly ICON_COLOR=0xff9399b2    # overlay2, dimmed for unfocused spaces
readonly ACCENT_COLOR=0xffedcb1b  # matches borders active_color in yabairc
readonly BAR_FONT="SF Pro"
