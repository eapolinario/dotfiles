#!/usr/bin/env bash

set -euo pipefail

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

stow_doom() {
  local script_dir
  script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

  if [[ ! -d "$script_dir/doom" ]]; then
    printf 'Doom configuration directory not found in %s.\n' "$script_dir" >&2
    exit 1
  fi

  local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
  local target_dir="$config_home/doom"

  mkdir -p "$target_dir"
  stow "${STOW_FLAGS[@]}" -vt "$target_dir" doom
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
  stow_doom
  if [[ "$DRY_RUN" == true ]]; then
    printf 'Dry run complete. Review stow output above for planned changes.\n'
  else
    printf 'Doom configuration symlinked into %s/doom.\n' "${XDG_CONFIG_HOME:-$HOME/.config}"
  fi
}

main "$@"
