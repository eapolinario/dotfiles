#!/usr/bin/env bash
set -euo pipefail

test_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
cd "$test_dir"

command -v emacs >/dev/null || {
  printf 'Missing test dependency: emacs\n' >&2
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

emacs --batch -Q \
  -l "$test_dir/org-download-test.el" \
  -l "$test_dir/agent-shell-test.el" \
  -f ert-run-tests-batch-and-exit
