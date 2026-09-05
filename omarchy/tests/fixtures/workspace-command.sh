#!/usr/bin/env bash
set -euo pipefail

case "${0##*/}" in
  hyprctl)
    printf '%s\n' "$1" >>"$SWAP_TEST_STATE/calls"
    if [[ "${SWAP_TEST_HYPRCTL_FAILURE:-}" == "$1" ]]; then
      printf 'Mock hyprctl query failed.\n' >&2
      exit 7
    fi
    case "$1" in
      activeworkspace)
        [[ "$2" == '-j' ]]
        printf '%s\n' "$SWAP_TEST_ACTIVE"
        ;;
      clients)
        [[ "$2" == '-j' ]]
        if [[ -s "$SWAP_TEST_STATE/clients-read" ]]; then
          printf 'Clients must only be queried once.\n' >&2
          exit 7
        fi
        printf '1\n' >"$SWAP_TEST_STATE/clients-read"
        printf '%s\n' "$SWAP_TEST_CLIENTS"
        ;;
      dispatch)
        printf '%s\n' "$2" >>"$SWAP_TEST_STATE/dispatches"
        if [[ "${SWAP_TEST_HYPRCTL_FAILURE:-}" == 'focus' && "$2" == hl.dsp.focus* ]] ||
          [[ "${SWAP_TEST_HYPRCTL_FAILURE:-}" == 'move' && "$2" == hl.dsp.window.move* ]]; then
          printf 'Mock hyprctl dispatch failed.\n' >&2
          exit 7
        fi
        printf 'ok\n'
        ;;
      *)
        printf 'Unexpected hyprctl arguments: %s\n' "$*" >&2
        exit 7
        ;;
    esac
    ;;
  jq)
    jq_calls=0
    read -r jq_calls <"$SWAP_TEST_STATE/jq-calls" || true
    jq_calls=$((jq_calls + 1))
    printf '%s\n' "$jq_calls" >"$SWAP_TEST_STATE/jq-calls"
    if [[ "${SWAP_TEST_JQ_FAILURE_AT:-}" == "$jq_calls" ]]; then
      printf 'Mock jq failed.\n' >&2
      exit 7
    fi
    exec "$SWAP_TEST_REAL_JQ" "$@"
    ;;
  zenity)
    printf 'zenity\n' >>"$SWAP_TEST_STATE/calls"
    printf '%s' "$SWAP_TEST_DEST"
    exit "${SWAP_TEST_ZENITY_STATUS:-0}"
    ;;
  swap-workspace.sh)
    printf 'workspace-binding-ok\n'
    ;;
  *)
    printf 'Unexpected mock command: %s\n' "$0" >&2
    exit 7
    ;;
esac
