#!/usr/bin/env bash
set -euo pipefail

test_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
test_file=$(realpath -- "${1:-"$test_dir/doom-config.el"}")
emacs_binary="${EMACS:-emacs}"
cd "$test_dir"

command -v "$emacs_binary" >/dev/null || {
  printf 'Missing test dependency: %s\n' "$emacs_binary" >&2
  exit 1
}

state_dir=".ert-home-$$"
mkdir -- "$state_dir"
trap 'rm -rf -- "$state_dir"' EXIT

export HOME="$test_dir/$state_dir/home"
export XDG_CONFIG_HOME="$test_dir/$state_dir/config"
export XDG_CACHE_HOME="$test_dir/$state_dir/cache"
export XDG_DATA_HOME="$test_dir/$state_dir/data"
export XDG_STATE_HOME="$test_dir/$state_dir/state"
export TMPDIR="$test_dir/$state_dir"
mkdir -p -- "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" "$XDG_STATE_HOME"

"$emacs_binary" --batch -Q \
  -l "$test_file" \
  -f ert-run-tests-batch-and-exit
