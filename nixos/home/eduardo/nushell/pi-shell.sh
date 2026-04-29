#!/usr/bin/env bash
set -euo pipefail

# Experimental Nushell bridge for pi.
#
# This intentionally keeps the implementation small and shell-local for now.
# Session storage layout, subcommands, and prompt parsing may change as the
# workflow matures.
readonly PI_SHELL_STATE_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/pi-shell"

require_cmd() {
  local command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    printf 'pi-shell: required command not found: %s\n' "$command_name" >&2
    exit 1
  fi
}

project_root() {
  pwd -P
}

project_key() {
  local root
  local hash

  root="$(project_root)"
  hash="$(printf '%s' "$root" | sha256sum)"
  printf '%s\n' "${hash%% *}"
}

project_session_dir() {
  printf '%s/sessions/%s\n' "$PI_SHELL_STATE_DIR" "$(project_key)"
}

project_active_file() {
  printf '%s/active/%s\n' "$PI_SHELL_STATE_DIR" "$(project_key)"
}

ensure_state_dirs() {
  mkdir -p "$PI_SHELL_STATE_DIR/active" "$(project_session_dir)"
}

get_active_session() {
  local active_file
  local session_file

  active_file="$(project_active_file)"
  if [[ ! -f "$active_file" ]]; then
    return 1
  fi

  session_file="$(<"$active_file")"
  if [[ -z "$session_file" ]]; then
    return 1
  fi

  printf '%s\n' "$session_file"
}

new_session_path() {
  local session_dir
  local timestamp

  session_dir="$(project_session_dir)"
  timestamp="$(date +%Y%m%d-%H%M%S)-$$-${RANDOM}"
  printf '%s/%s.jsonl\n' "$session_dir" "$timestamp"
}

set_active_session() {
  local session_file="$1"

  printf '%s\n' "$session_file" >"$(project_active_file)"
}

ensure_active_session() {
  local session_file

  if session_file="$(get_active_session 2>/dev/null)"; then
    printf '%s\n' "$session_file"
    return 0
  fi

  session_file="$(new_session_path)"
  set_active_session "$session_file"
  printf '%s\n' "$session_file"
}

join_words() {
  local result=""
  local word

  for word in "$@"; do
    if [[ -n "$result" ]]; then
      result+=" "
    fi
    result+="$word"
  done

  printf '%s\n' "$result"
}

run_pi() {
  local session_file="$1"
  shift
  local -a command=(pi --session "$session_file")

  command+=("$@")
  "${command[@]}"
}

run_pi_prompt() {
  local session_file="$1"
  shift
  local -a file_args=()
  local -a prompt_parts=()
  local arg
  local prompt_text

  for arg in "$@"; do
    if [[ "$arg" == @* ]]; then
      file_args+=("$arg")
    else
      prompt_parts+=("$arg")
    fi
  done

  if ((${#prompt_parts[@]} == 0)); then
    printf 'pi-shell: prompt text is required\n' >&2
    exit 1
  fi

  prompt_text="$(join_words "${prompt_parts[@]}")"
  run_pi "$session_file" -p "${file_args[@]}" "$prompt_text"
}

cmd_prompt() {
  local session_file

  if (($# == 0)); then
    printf 'Usage: : <prompt>\n' >&2
    exit 1
  fi

  session_file="$(ensure_active_session)"
  run_pi_prompt "$session_file" "$@"
}

cmd_new() {
  local session_file

  session_file="$(new_session_path)"
  set_active_session "$session_file"

  if (($# == 0)); then
    printf 'pi-shell: started new session %s\n' "$session_file"
    return 0
  fi

  run_pi_prompt "$session_file" "$@"
}

cmd_tui() {
  local session_file

  session_file="$(ensure_active_session)"
  run_pi "$session_file"
}

cmd_session() {
  printf '%s\n' "$(ensure_active_session)"
}

cmd_help() {
  cat <<'EOF'
EXPERIMENTAL: pi-shell is an early Nushell integration for pi.
Behavior, session semantics, and command names may change.

pi-shell commands:
  prompt <text...>   Send a prompt to the active pi session for this directory
  new [text...]      Start a fresh pi session for this directory
  tui                Open pi's TUI for the active session
  session            Print the active session file path
  help               Show this help text

Nushell shortcuts:
  : <prompt>
  :new [prompt]
  :tui
  :session

File attachments:
  Prefix files with @ to pass them through to pi, e.g.:
    : review @flake.nix @home/eduardo/default.nix
EOF
}

main() {
  local subcommand="${1:-help}"

  require_cmd pi
  require_cmd sha256sum
  ensure_state_dirs

  case "$subcommand" in
    prompt)
      shift
      cmd_prompt "$@"
      ;;
    new)
      shift
      cmd_new "$@"
      ;;
    tui)
      shift
      cmd_tui "$@"
      ;;
    session)
      shift
      cmd_session "$@"
      ;;
    help|-h|--help)
      cmd_help
      ;;
    *)
      printf 'pi-shell: unknown subcommand: %s\n' "$subcommand" >&2
      cmd_help >&2
      exit 1
      ;;
  esac
}

main "$@"
