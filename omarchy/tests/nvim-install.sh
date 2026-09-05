#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
readonly REPO_DIR
TEST_DIR="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-nvim-test.XXXXXXXX")"
readonly TEST_DIR
trap 'rm -r -- "$TEST_DIR"' EXIT

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

snapshot() {
  find "$1" -printf '%y %P %l\n' | LC_ALL=C sort
  find "$1" -type f -exec sha256sum {} + | LC_ALL=C sort
}

fixture_repo="$TEST_DIR/repo"
mkdir -p "$fixture_repo/omarchy" "$fixture_repo/common" "$TEST_DIR/bin"
cp "$REPO_DIR/omarchy/install.sh" "$fixture_repo/omarchy/install.sh"
cp -R "$REPO_DIR/common/nvim" "$fixture_repo/common/nvim"
source_root="$fixture_repo/common/nvim"
installer="$fixture_repo/omarchy/install.sh"

export HOME="$TEST_DIR/home with spaces"
export XDG_CONFIG_HOME="$HOME/custom-config"
export OMARCHY_PATH="$TEST_DIR/omarchy"
export SYSTEMCTL_MARKER="$TEST_DIR/systemctl-called"
export PATH="$TEST_DIR/bin:$PATH"
# The fixture expands this variable when executed.
# shellcheck disable=SC2016
printf '#!/usr/bin/env bash\nprintf called > "$SYSTEMCTL_MARKER"\nexit 1\n' >"$TEST_DIR/bin/systemctl"
chmod +x "$TEST_DIR/bin/systemctl"

nvim_dir="$XDG_CONFIG_HOME/nvim"
mkdir -p "$OMARCHY_PATH" "$nvim_dir/lua/config" "$nvim_dir/lua/plugins" \
  "$HOME/.local/state/omarchy/current/theme"
printf 'return {}\n' >"$nvim_dir/lua/config/remote_clipboard.lua"
printf 'return {}\n' >"$nvim_dir/lua/plugins/personal.lua"
printf 'return {}\n' >"$HOME/.local/state/omarchy/current/theme/neovim.lua"
ln -s '../../../../.local/state/omarchy/current/theme/neovim.lua' "$nvim_dir/lua/plugins/theme.lua"
printf '{"host":"local"}\n' >"$nvim_dir/lazy-lock.json"
printf '{"host":"shared"}\n' >"$source_root/lazy-lock.json"
printf 'error("shared theme must never be linked on Omarchy")\n' >"$source_root/lua/plugins/theme.lua"
cp "$source_root/init.lua" "$nvim_dir/init.lua"
printf 'personal options to preserve in backup\n' >"$TEST_DIR/original-options"
cp "$TEST_DIR/original-options" "$nvim_dir/lua/config/options.lua"

# The old package may already have disappeared from the checkout.
legacy_root="$fixture_repo/omarchy/nvim/.config/nvim"
for rel in lazyvim.json lua/plugins/fff.lua lua/plugins/gitsigns-blame.lua; do
  ln -s "$(realpath -m --relative-to="$(dirname "$nvim_dir/$rel")" "$legacy_root/$rel")" "$nvim_dir/$rel"
done

before="$(snapshot "$XDG_CONFIG_HOME")"
source_before="$(snapshot "$source_root")"
bash "$installer" --nvim-only --dry-run
[[ "$(snapshot "$XDG_CONFIG_HOME")" == "$before" ]] || fail 'dry-run changed configuration'

bash "$installer" --nvim-only
for rel in init.lua lazyvim.json lua/config/options.lua lua/config/platform.lua \
  lua/plugins/fff.lua lua/plugins/gitsigns-blame.lua lua/plugins/telescope.lua; do
  [[ -L "$nvim_dir/$rel" && "$(realpath "$nvim_dir/$rel")" == "$source_root/$rel" ]] ||
    fail "not linked to shared config: $rel"
done
for rel in . lua lua/config lua/plugins; do
  [[ -d "$nvim_dir/$rel" && ! -L "$nvim_dir/$rel" ]] || fail "directory was folded: $rel"
done
[[ ! -L "$nvim_dir/lazy-lock.json" ]] || fail 'lockfile was stowed'
[[ "$(readlink "$nvim_dir/lua/plugins/theme.lua")" == '../../../../.local/state/omarchy/current/theme/neovim.lua' ]] ||
  fail 'theme link changed'
[[ -f "$nvim_dir/lua/plugins/personal.lua" && ! -L "$nvim_dir/lua/plugins/personal.lua" ]] ||
  fail 'unrelated plugin changed'
[[ -f "$nvim_dir/lua/config/remote_clipboard.lua" && ! -L "$nvim_dir/lua/config/remote_clipboard.lua" ]] ||
  fail 'clipboard support changed'
[[ ! -e "$SYSTEMCTL_MARKER" ]] || fail 'nvim-only invoked systemctl'
[[ "$(snapshot "$source_root")" == "$source_before" ]] || fail 'installer modified shared source'

backup_dir="$(find "$XDG_CONFIG_HOME" -maxdepth 1 -type d -name 'nvim.backup.*')"
[[ -d "$backup_dir" ]] || fail 'expected exactly one backup directory'
cmp "$TEST_DIR/original-options" "$backup_dir/lua/config/options.lua"
cmp "$source_root/init.lua" "$backup_dir/init.lua"
[[ -L "$backup_dir/lazyvim.json" ]] || fail 'legacy symlink was not backed up'

before="$(snapshot "$XDG_CONFIG_HOME")"
bash "$installer" --nvim-only
bash "$installer" --nvim-only --dry-run
[[ "$(snapshot "$XDG_CONFIG_HOME")" == "$before" ]] || fail 'repeat installation was not idempotent'

# A conflict discovered after another replaceable file must not cause a partial migration.
rm -- "$nvim_dir/init.lua" "$nvim_dir/lua/config/keymaps.lua"
cp "$TEST_DIR/original-options" "$nvim_dir/init.lua"
ln -s "$TEST_DIR/unrelated.lua" "$nvim_dir/lua/config/keymaps.lua"
before="$(snapshot "$XDG_CONFIG_HOME")"
if bash "$installer" --nvim-only >"$TEST_DIR/conflict.log" 2>&1; then
  fail 'accepted unrelated symlink'
fi
grep -q 'Refusing to replace unrelated Neovim symlink' "$TEST_DIR/conflict.log"
[[ "$(snapshot "$XDG_CONFIG_HOME")" == "$before" ]] || fail 'conflict caused a partial migration'

rm -- "$nvim_dir/lua/config/keymaps.lua"
ln -s "$source_root/lua/config/keymaps.lua" "$nvim_dir/lua/config/keymaps.lua"
mv "$nvim_dir/lua/plugins" "$TEST_DIR/plugins"
ln -s "$TEST_DIR/plugins" "$nvim_dir/lua/plugins"
before="$(snapshot "$XDG_CONFIG_HOME")"
if bash "$installer" --nvim-only --dry-run >"$TEST_DIR/directory.log" 2>&1; then
  fail 'accepted a directory symlink'
fi
grep -q 'Expected a real Neovim directory' "$TEST_DIR/directory.log"
[[ "$(snapshot "$XDG_CONFIG_HOME")" == "$before" ]] || fail 'directory conflict changed configuration'

export XDG_CONFIG_HOME="$TEST_DIR/new-config"
if bash "$installer" --nvim-only --dry-run >"$TEST_DIR/missing-support.log" 2>&1; then
  fail 'accepted missing Omarchy support files'
fi
grep -q 'Omarchy Neovim support files are missing' "$TEST_DIR/missing-support.log"
[[ ! -e "$XDG_CONFIG_HOME" ]] || fail 'failed preflight created directories'

export OMARCHY_PATH="$TEST_DIR/not-omarchy"
bash "$installer" --nvim-only --dry-run
[[ ! -e "$XDG_CONFIG_HOME" ]] || fail 'fresh dry-run created directories'
bash "$installer" --nvim-only
[[ -L "$XDG_CONFIG_HOME/nvim/init.lua" ]] || fail 'fresh installation did not link shared config'
[[ ! -e "$XDG_CONFIG_HOME/nvim/lazy-lock.json" ]] || fail 'fresh installation stowed a lockfile'
[[ ! -e "$XDG_CONFIG_HOME/nvim/lua/plugins/theme.lua" ]] || fail 'fresh installation stowed a theme'
[[ ! -e "$SYSTEMCTL_MARKER" ]] || fail 'nvim-only invoked systemctl'

printf 'Neovim installer tests passed\n'
