#!/usr/bin/env bash

set -euo pipefail

if ! command -v herdr-app >/dev/null; then
  printf 'Skipping Panopticon render checks (herdr-app not installed).\n'
  exit 0
fi

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source_dir="${XDG_CONFIG_HOME:-$HOME/.config}/herdr-app/mods/panopticon"
if [[ ! -f "$source_dir/main.js" || ! -f "$source_dir/mod.json" ]]; then
  printf 'Skipping Panopticon render checks (v0.3.0 mod is not installed locally).\n'
  exit 0
fi
test_dir="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-panopticon.XXXXXXXX")"
trap 'rm -r -- "$test_dir"' EXIT
export HERDR_APP_HOME="$test_dir/home"
mod_dir="$HERDR_APP_HOME/mods/panopticon"
mkdir -p "$mod_dir" "$test_dir/fixture"

# Render the individually linked files as installed; check writes its own editor support.
ln -s "$source_dir/main.js" "$source_dir/mod.json" "$mod_dir/"
herdr-app check "$mod_dir"

# Herdr confines relative imports to a mod's directory, so copy the subject beside the fixture.
cp "$source_dir/main.js" "$test_dir/fixture/panopticon.js"
cp "$repo_dir/omarchy/tests/fixtures/panopticon.js" "$test_dir/fixture/main.js"
herdr-app check "$test_dir/fixture"
