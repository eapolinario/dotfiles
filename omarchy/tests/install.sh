#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_DIR="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-install-test.XXXXXXXX")"
REAL_STOW="$(command -v stow)"
readonly REPO_DIR TEST_DIR REAL_STOW
export REAL_STOW
trap 'rm -r -- "$TEST_DIR"' EXIT

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

snapshot() {
  if [[ ! -e "$1" ]]; then
    printf 'missing\n'
    return
  fi
  find "$1" -printf '%y %m %P %l\n' | LC_ALL=C sort
  find "$1" -type f -exec sha256sum {} + | LC_ALL=C sort
}

succeeds() {
  if ! bash "$installer" "$@" >"$TEST_DIR/install.log" 2>&1; then
    sed -n '1,160p' "$TEST_DIR/install.log" >&2
    fail "installer failed: $*"
  fi
}

fails() {
  local message="$1"
  shift
  if bash "$installer" "$@" >"$TEST_DIR/install.log" 2>&1; then
    fail "installer unexpectedly succeeded: $*"
  fi
  if ! grep -q -- "$message" "$TEST_DIR/install.log"; then
    sed -n '1,160p' "$TEST_DIR/install.log" >&2
    fail "missing error: $message"
  fi
}

new_home() {
  export HOME="$TEST_DIR/$1 home"
  export XDG_CONFIG_HOME="$HOME/custom config"
  export OMARCHY_PATH="$TEST_DIR/not-omarchy"
  export SYSTEMCTL_MARKER="$TEST_DIR/$1.systemctl"
}

fixture="$TEST_DIR/repo"
mkdir -p "$fixture/omarchy" "$fixture/common/authinfo" "$TEST_DIR/bin"
cp "$REPO_DIR/omarchy/install.sh" "$fixture/omarchy/"
cp -R "$REPO_DIR/omarchy/hypr" "$REPO_DIR/omarchy/ghostty" "$REPO_DIR/omarchy/systemd" "$fixture/omarchy/"
for component in doom nvim copilot claude pi skills; do
  cp -R "$REPO_DIR/common/$component" "$fixture/common/"
done
# Do not copy real credentials, even when running from an unlocked checkout.
printf '\0GITCRYPT\0fixture ciphertext\n' >"$fixture/common/authinfo/.authinfo"
installer="$fixture/omarchy/install.sh"

printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'printf "%s\n" "$*" >> "$SYSTEMCTL_MARKER"' \
  'if [[ "$*" == *show-environment* && "${MOCK_MANAGER_FAIL:-0}" == 1 ]]; then exit 1; fi' \
  'if [[ "$*" == *enable* && "${MOCK_ENABLE_FAIL:-0}" == 1 ]]; then exit 1; fi' \
  'exit 0' >"$TEST_DIR/bin/systemctl"
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'if [[ "${MOCK_SKIP_STOW:-0}" == 1 ]]; then exit 0; fi' \
  '"$REAL_STOW" "$@"' \
  'if [[ "${*: -1}" == "${MOCK_FAIL_PACKAGE:-}" ]]; then exit 23; fi' >"$TEST_DIR/bin/stow"
printf '%s\n' '#!/usr/bin/env bash' 'exit 0' >"$TEST_DIR/bin/uvx"
chmod +x "$TEST_DIR/bin/"*
export PATH="$TEST_DIR/bin:$PATH"

new_home fresh
source_before="$(snapshot "$fixture")"
succeeds --help
succeeds --dry-run
[[ ! -e "$HOME" && ! -e "$SYSTEMCTL_MARKER" ]] || fail 'fresh full dry-run had side effects'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'dry-run changed source'
fails 'Unknown component' --only nonexistent
fails 'requires a value' --only
fails 'empty entries' --only doom,
fails 'Duplicate component' --only doom,doom
fails 'cannot be combined' --nvim-only --enable-services
fails 'requires selecting' --only ghostty --enable-services
fails 'Required authinfo' --require-secrets --dry-run
[[ ! -e "$HOME" ]] || fail 'argument/secret failure created HOME'

# Detect a late conflict before moving earlier replaceable files.
mkdir -p "$XDG_CONFIG_HOME/doom" "$HOME/.claude" "$HOME/.copilot"
printf 'original Doom config\n' >"$XDG_CONFIG_HOME/doom/config.el"
printf 'local Copilot state\n' >"$HOME/.copilot/session-store.db"
printf 'existing credentials fixture\n' >"$HOME/.authinfo"
ln -s "$TEST_DIR/unrelated.json" "$HOME/.claude/settings.json"
before="$(snapshot "$HOME")"
fails 'Refusing to replace unrelated symlink' --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'failed dry-run changed HOME'
fails 'Refusing to replace unrelated symlink'
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'late conflict partially installed earlier packages'
rm -- "$HOME/.claude/settings.json"

succeeds
[[ ! -e "$SYSTEMCTL_MARKER" ]] || fail 'default install called systemctl'
[[ ! -e "$HOME/.config" ]] || fail 'custom XDG install wrote to ~/.config'
for file in doom/config.el hypr/bindings.lua ghostty/config nvim/init.lua systemd/user/grasp.service; do
  [[ -L "$XDG_CONFIG_HOME/$file" ]] || fail "missing XDG link: $file"
done
[[ ! -e "$XDG_CONFIG_HOME/systemd/user/downloads-clean-at-login.service" ]] ||
  fail 'default install selected cleanup service'
[[ ! -e "$XDG_CONFIG_HOME/user-tmpfiles.d/empty-downloads.conf" ]] || fail 'installed a global cleanup rule'
[[ ! -e "$XDG_CONFIG_HOME/dotfiles/downloads-cleanup.conf" ]] || fail 'default install enabled cleanup'
[[ ! -e "$XDG_CONFIG_HOME/doom/tests" ]] || fail 'installed Doom tests'
grep -q 'original Doom config' "$XDG_CONFIG_HOME"/doom.backup.*/config.el
grep -q 'local Copilot state' "$HOME/.copilot/session-store.db"
grep -q 'existing credentials fixture' "$HOME/.authinfo"
[[ ! -L "$HOME/.authinfo" ]] || fail 'linked locked credentials'
for agent_dir in "$HOME/.copilot" "$HOME/.claude" "$HOME/.pi/agent"; do
  [[ -d "$agent_dir/skills" && ! -L "$agent_dir/skills" ]] || fail 'folded agent state or skills directory'
  for skill_file in "$fixture"/common/skills/*/SKILL.md; do
    name="$(basename "$(dirname "$skill_file")")"
    [[ "$(realpath "$agent_dir/skills/$name")" == "$(dirname "$skill_file")" ]] ||
      fail "missing shared skill: $name"
  done
done
before="$(snapshot "$HOME")"
succeeds
succeeds --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'repeat full install was not idempotent'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'install changed source'

# New skills are discovered without editing static aliases or a second manifest.
mkdir -p "$fixture/common/skills/later-skill"
printf '%s\n' '---' 'name: later-skill' 'description: Fixture' '---' >"$fixture/common/skills/later-skill/SKILL.md"
succeeds --only copilot,claude,pi
for agent_dir in "$HOME/.copilot" "$HOME/.claude" "$HOME/.pi/agent"; do
  [[ -L "$agent_dir/skills/later-skill" ]] || fail 'new shared skill was not discovered'
done

# Explicit secret installation is independent of services and does not print contents.
printf 'machine example.invalid login fixture password not-a-real-credential\n' >"$fixture/common/authinfo/.authinfo"
succeeds --only authinfo --require-secrets
[[ -L "$HOME/.authinfo" ]] || fail 'did not link unlocked authinfo'
if grep -q 'not-a-real-credential' "$TEST_DIR/install.log"; then
  fail 'printed credential contents'
fi
printf '\0GITCRYPT\0fixture ciphertext\n' >"$fixture/common/authinfo/.authinfo"

new_home cleanup
mkdir -p "$HOME/Downloads \"quoted\" 100%"
downloads="$HOME/Downloads \"quoted\" 100%"
printf 'keep this fixture\n' >"$downloads/keep"
fails 'Cleanup options require' --downloads-age 0
fails 'Invalid retention' --only downloads-cleanup --downloads-age tomorrow
fails 'Refusing cleanup' --only downloads-cleanup --downloads-dir "$HOME"
fails 'Refusing cleanup' --only downloads-cleanup --downloads-dir /
fails 'Refusing cleanup' --only downloads-cleanup --downloads-dir "$XDG_CONFIG_HOME/hypr"
fails 'glob characters' --only downloads-cleanup --downloads-dir "$HOME/Down*"
before="$(snapshot "$HOME")"
succeeds --only downloads-cleanup --downloads-dir "$downloads" --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'cleanup dry-run wrote files'
succeeds --only downloads-cleanup --downloads-dir "$downloads"
private_rule="$XDG_CONFIG_HOME/dotfiles/downloads-cleanup.conf"
[[ -f "$private_rule" && ! -L "$private_rule" ]] || fail 'missing private cleanup config'
grep -q -- ' - - - 7d$' "$private_rule"
grep -q -- '100%%' "$private_rule"
[[ ! -e "$XDG_CONFIG_HOME/user-tmpfiles.d" ]] || fail 'cleanup rule entered global search path'
[[ ! -e "$SYSTEMCTL_MARKER" && -f "$downloads/keep" ]] || fail 'cleanup activated without permission'
# Parse with real tmpfiles but exclude the cleanup directory; no deletion occurs.
systemd-tmpfiles --user --clean --prefix="$TEST_DIR/never-clean" "$private_rule"
[[ -f "$downloads/keep" ]] || fail 'tmpfiles parser fixture was not isolated'
before="$(snapshot "$HOME")"
succeeds --only downloads-cleanup --downloads-dir "$downloads"
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'cleanup repeat created another backup'
succeeds --only downloads-cleanup --downloads-dir "$downloads" --downloads-age 0
grep -q -- ' - - - 0$' "$private_rule"
[[ -f "$downloads/keep" ]] || fail 'configuring zero retention performed cleanup'

new_home manager
export MOCK_MANAGER_FAIL=1
fails 'User systemd instance unavailable' --only grasp --enable-services
[[ ! -e "$HOME" ]] || fail 'manager preflight failure installed files'
unset MOCK_MANAGER_FAIL
fails 'before enabling Grasp' --only grasp --enable-services
mkdir -p "$HOME/org"
succeeds --only grasp --enable-services
grep -q -- '--user daemon-reload' "$SYSTEMCTL_MARKER"
grep -q -- '--user enable --now grasp.service' "$SYSTEMCTL_MARKER"
export MOCK_ENABLE_FAIL=1
fails 'activation failed' --only grasp --enable-services
[[ -L "$XDG_CONFIG_HOME/systemd/user/grasp.service" ]] || fail 'service failure claimed to undo file install'
unset MOCK_ENABLE_FAIL

# A tool failure after it has created links must restore previous components too.
new_home rollback
export MOCK_FAIL_PACKAGE=nvim
fails 'Installation failed; restoring' --only ghostty,nvim
[[ ! -e "$HOME" ]] || fail 'fresh rollback left a partial configuration tree'
mkdir -p "$XDG_CONFIG_HOME/ghostty" "$XDG_CONFIG_HOME/nvim/lua/config"
printf 'old terminal config\n' >"$XDG_CONFIG_HOME/ghostty/config"
printf 'old editor config\n' >"$XDG_CONFIG_HOME/nvim/lua/config/options.lua"
fails 'Installation failed; restoring' --only ghostty,nvim
grep -q 'old terminal config' "$XDG_CONFIG_HOME/ghostty/config"
grep -q 'old editor config' "$XDG_CONFIG_HOME/nvim/lua/config/options.lua"
[[ ! -L "$XDG_CONFIG_HOME/ghostty/config" && ! -e "$XDG_CONFIG_HOME/nvim/init.lua" ]] ||
  fail 'rollback left managed links in place'
unset MOCK_FAIL_PACKAGE
new_home silent-stow
export MOCK_SKIP_STOW=1
fails 'Stow did not install' --only ghostty
[[ ! -e "$HOME" ]] || fail 'incomplete Stow rollback left files'
unset MOCK_SKIP_STOW

# Doctor must not bootstrap packages, create directories, or call systemctl.
new_home doctor
mkdir -p "$TEST_DIR/doctor-bin"
for cmd in dirname realpath uname; do
  ln -s "$(command -v "$cmd")" "$TEST_DIR/doctor-bin/$cmd"
done
ln -s "$REAL_STOW" "$TEST_DIR/doctor-bin/stow"
if PATH="$TEST_DIR/doctor-bin" /usr/bin/bash "$installer" --only ghostty --doctor >"$TEST_DIR/doctor.log" 2>&1; then
  fail 'doctor did not fail for a missing required runtime'
fi
grep -q '\[MISSING\] ghostty' "$TEST_DIR/doctor.log"
[[ ! -e "$HOME" && ! -e "$SYSTEMCTL_MARKER" ]] || fail 'doctor mutated the environment'

printf 'Omarchy installer tests passed\n'
