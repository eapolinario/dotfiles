#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR

DRY_RUN=false
declare -a STOW_FLAGS=()

usage() {
  cat <<'EOF'
Usage: install.sh [OPTIONS]

Options:
  -n, --dry-run   Pass --no/--simulate to stow (no filesystem changes)
  -h, --help      Show this message and exit
EOF
}

ensure_linux() {
  if [[ "$(uname -s)" != "Linux" ]]; then
    printf 'This installer is intended for Linux hosts.\n' >&2
    exit 1
  fi
}

require_cmd() {
  local cmd="$1"
  local hint="${2:-}"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    if [[ -n "$hint" ]]; then
      printf 'Missing dependency: %s. %s\n' "$cmd" "$hint" >&2
    else
      printf 'Missing dependency: %s\n' "$cmd" >&2
    fi
    exit 1
  fi
}

run_user_systemctl() {
  if [[ "$DRY_RUN" == true ]]; then
    printf '[DRY RUN] systemctl --user'
    printf ' %q' "$@"
    printf '\n'
  else
    systemctl --user "$@"
  fi
}

remove_target_if_identical() {
  local target="$1"
  local source="$2"

  if [[ -L "$target" || ! -e "$target" ]]; then
    return
  fi

  if [[ -d "$target" ]]; then
    printf 'Unexpected directory at %s. Remove it before continuing.\n' "$target" >&2
    exit 1
  fi

  if cmp -s "$target" "$source"; then
    if [[ "$DRY_RUN" == true ]]; then
      printf '[DRY RUN] would remove %s to replace with managed symlink.\n' "$target"
    else
      rm "$target"
    fi
  else
    printf 'Existing %s differs from the tracked version. Remove or back it up before rerunning.\n' "$target" >&2
    exit 1
  fi
}

stow_doom() {
  if [[ ! -d "$SCRIPT_DIR/../common/doom" ]]; then
    printf 'Doom configuration directory not found in %s.\n' "$SCRIPT_DIR/../common" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local target_dir="$config_home/doom"

  mkdir -p "$target_dir"
  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR/../common" -vt "$target_dir" doom
}

stow_authinfo() {
  if [[ ! -d "$SCRIPT_DIR/../common/authinfo" ]]; then
    printf 'authinfo directory not found in %s.\n' "$SCRIPT_DIR/../common" >&2
    exit 1
  fi

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR/../common" -vt "$HOME" authinfo
}

stow_systemd_configs() {
  if [[ ! -d "$SCRIPT_DIR/systemd" ]]; then
    printf 'Systemd configuration directory not found in %s.\n' "$SCRIPT_DIR" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  mkdir -p "$config_home/systemd/user" "$config_home/user-tmpfiles.d"

  local service_source="$SCRIPT_DIR/systemd/.config/systemd/user/downloads-clean-at-login.service"
  local service_target="$config_home/systemd/user/downloads-clean-at-login.service"
  local tmpfiles_source="$SCRIPT_DIR/systemd/.config/user-tmpfiles.d/empty-downloads.conf"
  local tmpfiles_target="$config_home/user-tmpfiles.d/empty-downloads.conf"

  remove_target_if_identical "$service_target" "$service_source"
  remove_target_if_identical "$tmpfiles_target" "$tmpfiles_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" systemd
}

# Omarchy 4 (quattro) reads Hyprland config from Lua, not the old *.conf files.
# The pre-quattro symlinks are left dangling by the upgrade, so clear them out
# before stowing the Lua replacements.
readonly -a LEGACY_HYPR_CONFS=(
  input.conf
  bindings.conf
  looknfeel.conf
  monitors.conf
)

remove_legacy_hypr_confs() {
  local hypr_dir="$1"
  local name target

  for name in "${LEGACY_HYPR_CONFS[@]}"; do
    target="$hypr_dir/$name"

    # Only reclaim symlinks this repo owns; a real file is the user's own config.
    if [[ -L "$target" && "$(readlink -f "$target")" == "$SCRIPT_DIR"/* ]]; then
      if [[ "$DRY_RUN" == true ]]; then
        printf '[DRY RUN] would remove stale symlink %s (quattro moved this to Lua).\n' "$target"
      else
        rm "$target"
        printf 'Removed stale symlink %s (quattro moved this to Lua).\n' "$target"
      fi
    fi
  done
}

stow_hypr_configs() {
  if [[ ! -d "$SCRIPT_DIR/hypr" ]]; then
    printf 'Hypr configuration directory not found in %s.\n' "$SCRIPT_DIR" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local hypr_dir="$config_home/hypr"

  mkdir -p "$hypr_dir"
  mkdir -p "$hypr_dir/scripts"

  remove_legacy_hypr_confs "$hypr_dir"

  # monitors.lua is deliberately not tracked: omarchy-hyprland-monitor-scaling
  # rewrites it with sed -i, which would replace a stowed symlink with a file.
  remove_target_if_identical "$hypr_dir/input.lua" "$SCRIPT_DIR/hypr/.config/hypr/input.lua"
  remove_target_if_identical "$hypr_dir/bindings.lua" "$SCRIPT_DIR/hypr/.config/hypr/bindings.lua"
  remove_target_if_identical "$hypr_dir/looknfeel.lua" "$SCRIPT_DIR/hypr/.config/hypr/looknfeel.lua"
  remove_target_if_identical "$hypr_dir/scripts/swap-workspace.sh" "$SCRIPT_DIR/hypr/.config/hypr/scripts/swap-workspace.sh"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" hypr
}


stow_uwsm_config() {
  # Omarchy 4 (quattro) ships as a system package and points
  # ~/.local/share/omarchy at the root-owned /usr/share/omarchy, so the old
  # ~/.local/share/omarchy/config/uwsm/default override is no longer writable
  # (or read). Only ~/.config/uwsm/default remains a user-owned override.
  local config_source="$SCRIPT_DIR/uwsm/.config/uwsm/default"

  if [[ ! -f "$config_source" ]]; then
    printf 'UWSM default file not found in %s.\n' "$SCRIPT_DIR/uwsm/.config/uwsm" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  mkdir -p "$config_home/uwsm"
  remove_target_if_identical "$config_home/uwsm/default" "$config_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" uwsm
}


stow_ghostty_config() {
  if [[ ! -d "$SCRIPT_DIR/ghostty" ]]; then
    return
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local ghostty_dir="$config_home/ghostty"

  mkdir -p "$ghostty_dir"
  remove_target_if_identical "$ghostty_dir/config" "$SCRIPT_DIR/ghostty/.config/ghostty/config"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" ghostty
}


stow_xcompose_config() {
  if [[ ! -f "$SCRIPT_DIR/xcompose/.XCompose" ]]; then
    return
  fi

  remove_target_if_identical "$HOME/.XCompose" "$SCRIPT_DIR/xcompose/.XCompose"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" xcompose
}


stow_hyprwhspr_config() {
  if [[ ! -f "$SCRIPT_DIR/hyprwhspr/.config/hyprwhspr/config.json" ]]; then
    printf 'hyprwhspr configuration file not found in %s.\n' "$SCRIPT_DIR/hyprwhspr/.config/hyprwhspr" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local hyprwhspr_dir="$config_home/hyprwhspr"
  local config_source="$SCRIPT_DIR/hyprwhspr/.config/hyprwhspr/config.json"
  local config_target="$hyprwhspr_dir/config.json"

  mkdir -p "$hyprwhspr_dir"
  remove_target_if_identical "$config_target" "$config_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" hyprwhspr
}


stow_nushell_config() {
  if [[ ! -f "$SCRIPT_DIR/nushell/.config/nushell/config.nu" ]]; then
    printf 'Nushell configuration file not found in %s.\n' "$SCRIPT_DIR/nushell/.config/nushell" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local nushell_dir="$config_home/nushell"

  mkdir -p "$nushell_dir"
  remove_target_if_identical "$nushell_dir/config.nu" "$SCRIPT_DIR/nushell/.config/nushell/config.nu"
  remove_target_if_identical "$nushell_dir/env.nu" "$SCRIPT_DIR/nushell/.config/nushell/env.nu"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" nushell
}

enable_downloads_clean_service() {
  if [[ "$DRY_RUN" == true ]]; then
    run_user_systemctl daemon-reload
    run_user_systemctl enable --now downloads-clean-at-login.service
    return
  fi

  if ! systemctl --user show-environment >/dev/null 2>&1; then
    printf 'User systemd instance not available; skipping downloads-clean-at-login.service enablement.\n' >&2
    return
  fi

  run_user_systemctl daemon-reload
  run_user_systemctl enable --now downloads-clean-at-login.service
  printf 'downloads-clean-at-login.service enabled for the user.\n'
}

enable_grasp_service() {
  if [[ "$DRY_RUN" == true ]]; then
    run_user_systemctl daemon-reload
    run_user_systemctl enable --now grasp.service
    return
  fi

  if ! systemctl --user show-environment >/dev/null 2>&1; then
    printf 'User systemd instance not available; skipping grasp.service enablement.\n' >&2
    return
  fi

  run_user_systemctl daemon-reload
  run_user_systemctl enable --now grasp.service
  printf 'grasp.service enabled for the user.\n'
  printf 'Ensure the browser extension is installed: https://github.com/karlicoss/grasp\n'
}

main() {
  ensure_linux

  while (( $# > 0 )); do
    case "$1" in
      -n|--dry-run)
        DRY_RUN=true
        STOW_FLAGS+=(-n)
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      *)
        printf 'Unknown option: %s\n\n' "$1" >&2
        usage >&2
        exit 1
        ;;
    esac
    shift
  done

  require_cmd stow "Install it via your package manager"
  require_cmd systemctl "Required to manage user services"

  stow_doom
  stow_authinfo
  stow_systemd_configs
  stow_hypr_configs
  stow_ghostty_config
  stow_xcompose_config
  stow_hyprwhspr_config
  stow_nushell_config
  stow_uwsm_config
  enable_downloads_clean_service
  enable_grasp_service

  if [[ "$DRY_RUN" == true ]]; then
    printf 'Dry run complete. Review stow and systemctl output above for planned changes.\n'
  else
    printf 'Dotfiles installed. Doom configuration symlinked and user services enabled.\n'
  fi
}

main "$@"
