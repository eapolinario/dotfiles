#!/usr/bin/env bash

set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

DRY_RUN=false
declare -a STOW_FLAGS=()

usage() {
  cat <<'EOF'
Usage: install_dotfiles_linux.sh [OPTIONS]

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
  if [[ ! -d "$SCRIPT_DIR/doom" ]]; then
    printf 'Doom configuration directory not found in %s.\n' "$SCRIPT_DIR" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local target_dir="$config_home/doom"

  mkdir -p "$target_dir"
  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$target_dir" doom
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

stow_hypr_configs() {
  if [[ ! -d "$SCRIPT_DIR/hypr" ]]; then
    printf 'Hypr configuration directory not found in %s.\n' "$SCRIPT_DIR" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local hypr_dir="$config_home/hypr"
  local input_source="$SCRIPT_DIR/hypr/.config/hypr/input.conf"
  local bindings_source="$SCRIPT_DIR/hypr/.config/hypr/bindings.conf"
  local input_target="$hypr_dir/input.conf"
  local bindings_target="$hypr_dir/bindings.conf"

  mkdir -p "$hypr_dir"

  remove_target_if_identical "$input_target" "$input_source"
  remove_target_if_identical "$bindings_target" "$bindings_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" hypr
}


stow_starship_config() {
  if [[ ! -f "$SCRIPT_DIR/starship/.config/starship.toml" ]]; then
    printf 'Starship configuration file not found in %s.\n' "$SCRIPT_DIR/starship/.config" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local config_target="$config_home/starship.toml"
  local config_source="$SCRIPT_DIR/starship/.config/starship.toml"

  mkdir -p "$config_home"
  remove_target_if_identical "$config_target" "$config_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" starship
}


stow_uwsm_config() {
  local uwsm_dir="$HOME/.local/share/omarchy/config/uwsm"
  local config_source="$SCRIPT_DIR/uwsm/.local/share/omarchy/config/uwsm/default"
  local config_target="$uwsm_dir/default"

  if [[ ! -f "$config_source" ]]; then
    printf 'UWSM default file not found in %s.\n' "$SCRIPT_DIR/uwsm/.local/share/omarchy/config/uwsm" >&2
    exit 1
  fi

  mkdir -p "$uwsm_dir"
  remove_target_if_identical "$config_target" "$config_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" uwsm
}


stow_eca_config() {
  if [[ ! -f "$SCRIPT_DIR/eca/.config/eca/config.json" ]]; then
    printf 'ECA configuration file not found in %s.\n' "$SCRIPT_DIR/eca/.config/eca" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local eca_dir="$config_home/eca"
  local config_source="$SCRIPT_DIR/eca/.config/eca/config.json"
  local config_target="$eca_dir/config.json"

  mkdir -p "$eca_dir"
  remove_target_if_identical "$config_target" "$config_source"

  stow "${STOW_FLAGS[@]}" -d "$SCRIPT_DIR" -vt "$HOME" eca
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
  stow_systemd_configs
  stow_hypr_configs
  stow_starship_config
  stow_eca_config
  stow_uwsm_config
  enable_downloads_clean_service

  if [[ "$DRY_RUN" == true ]]; then
    printf 'Dry run complete. Review stow and systemctl output above for planned changes.\n'
  else
    printf 'Dotfiles installed. Doom configuration symlinked and downloads-clean-at-login.service ensured.\n'
  fi
}

main "$@"
