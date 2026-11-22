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
cp -R "$REPO_DIR/omarchy/hypr" "$REPO_DIR/omarchy/ghostty" "$REPO_DIR/omarchy/herdr-app" \
  "$REPO_DIR/omarchy/systemd" "$REPO_DIR/omarchy/pi" "$fixture/omarchy/"
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
mkdir -p "$TEST_DIR/panopticon-release/mods/panopticon"
printf '// pinned fixture main\n' >"$TEST_DIR/panopticon-release/mods/panopticon/main.js"
printf '{"title":"Panopticon","place":"none"}\n' >"$TEST_DIR/panopticon-release/mods/panopticon/mod.json"
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'output=' 'url=' 'while (($#)); do' '  case "$1" in' \
  '    -o) output=$2; shift 2 ;;' '    http://*|https://*) url=$1; shift ;;' '    *) shift ;;' '  esac' 'done' \
  'printf "%s\n" "$url" > "${PANOPTICON_TEST_CURL_LOG:?}"' \
  '[[ -n "$output" && -n "$PANOPTICON_TEST_INSTALLER" ]]' \
  'cp "$PANOPTICON_TEST_INSTALLER" "$output"' >"$TEST_DIR/bin/curl"
printf '%s\n' '#!/usr/bin/env bash' 'set -euo pipefail' \
  'if [[ "${1:-}" == check ]]; then exit 0; fi' 'exit 0' >"$TEST_DIR/bin/herdr-app"
cat >"$TEST_DIR/panopticon-release-installer.sh" <<'SH'
#!/bin/sh
if [ -z "${PANOPTICON_FIXTURE_BASH_REEXEC:-}" ]; then
  export PANOPTICON_FIXTURE_BASH_REEXEC=1
  exec bash "$0" "$@"
fi
set -euo pipefail
app_home=
ref=
while (($#)); do
  case "$1" in
    --app-home) app_home=$2; shift 2 ;;
    --ref) ref=$2; shift 2 ;;
    *) exit 2 ;;
  esac
done
printf '%s\n%s\n' "$app_home" "$ref" >>"${PANOPTICON_TEST_RELEASE_LOG:?}"
[[ "$ref" == v0.3.0 ]]
if [[ "${PANOPTICON_TEST_RELEASE_FAIL:-0}" == 1 ]]; then exit 22; fi
if [[ -e "$app_home/mods/panopticon" || -L "$app_home/mods/panopticon" ]]; then
  if [[ ! -f "$app_home/mods/panopticon/.herdr-panopticon" || \
    -L "$app_home/mods/panopticon/.herdr-panopticon" ]]; then exit 23; fi
  owner=
  IFS= read -r owner <"$app_home/mods/panopticon/.herdr-panopticon" || exit 23
  [[ "$owner" == 'herdr-panopticon installer' ]] || exit 23
fi
mkdir -p "$app_home/mods/panopticon"
cp "$PANOPTICON_TEST_RELEASE_SOURCE/main.js" "$app_home/mods/panopticon/main.js"
cp "$PANOPTICON_TEST_RELEASE_SOURCE/mod.json" "$app_home/mods/panopticon/mod.json"
printf 'herdr-panopticon installer\n' >"$app_home/mods/panopticon/.herdr-panopticon"
SH
chmod +x "$TEST_DIR/bin/"* "$TEST_DIR/panopticon-release-installer.sh"
export PANOPTICON_TEST_INSTALLER="$TEST_DIR/panopticon-release-installer.sh"
export PANOPTICON_TEST_CURL_LOG="$TEST_DIR/panopticon-curl.log"
export PANOPTICON_TEST_RELEASE_LOG="$TEST_DIR/panopticon-release.log"
export PANOPTICON_TEST_RELEASE_SOURCE="$TEST_DIR/panopticon-release/mods/panopticon"
export PATH="$TEST_DIR/bin:$PATH"

new_home fresh
source_before="$(snapshot "$fixture")"
succeeds --help
succeeds --only authinfo
grep -q 'No files were selected for installation' "$TEST_DIR/install.log"
[[ ! -e "$HOME" ]] || fail 'skipped authinfo install created HOME'
succeeds --dry-run
[[ ! -e "$HOME" && ! -e "$SYSTEMCTL_MARKER" ]] || fail 'fresh full dry-run had side effects'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'dry-run changed source'
fails 'Unknown component' --only nonexistent
fails 'Unknown component: herdr' --only herdr
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
for file in doom/config.el hypr/bindings.lua ghostty/config herdr-app/keys.json \
  herdr-app/settings.json nvim/init.lua systemd/user/grasp.service; do
  [[ -L "$XDG_CONFIG_HOME/$file" ]] || fail "missing XDG link: $file"
done
[[ ! -e "$XDG_CONFIG_HOME/systemd/user/downloads-clean-at-login.service" ]] ||
  fail 'default install selected cleanup service'
[[ ! -e "$XDG_CONFIG_HOME/systemd/user/battery-charge-limit.service" &&
  ! -e "$XDG_CONFIG_HOME/systemd/system/battery-charge-limit.service" ]] ||
  fail 'default install selected the opt-in system battery service'
[[ ! -e "$XDG_CONFIG_HOME/user-tmpfiles.d/empty-downloads.conf" ]] || fail 'installed a global cleanup rule'
[[ ! -e "$XDG_CONFIG_HOME/dotfiles/downloads-cleanup.conf" ]] || fail 'default install enabled cleanup'
[[ ! -e "$XDG_CONFIG_HOME/doom/tests" ]] || fail 'installed Doom tests'
[[ "$(realpath "$HOME/.pi/agent/settings.json")" == "$fixture/omarchy/pi/.pi/agent/settings.json" ]] ||
  fail 'Pi settings did not use the Omarchy configuration'
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
# Claude no longer enables the plugin; Pi uses the independently shared skill.
if grep -qi 'caveman' "$HOME/.claude/settings.json"; then
  fail 'Claude settings still reference Caveman'
fi
[[ ! -e "$HOME/.claude/hooks/caveman-statusline.sh" &&
  ! -L "$HOME/.claude/hooks/caveman-statusline.sh" ]] || fail 'installed the removed Caveman wrapper'
[[ -r "$HOME/.pi/agent/skills/caveman/SKILL.md" ]] || fail 'Pi shared Caveman skill is missing'
if grep -qF 'plugins/marketplaces/caveman' "$HOME/.pi/agent/prompts/caveman.md" \
  "$fixture/common/pi/.pi/agent/settings.json"; then
  fail 'Pi still depends on the removed Claude marketplace'
fi
before="$(snapshot "$HOME")"
succeeds
succeeds --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'repeat full install was not idempotent'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'install changed source'

# The desktop app links personal config; Panopticon comes only from pinned release v0.3.0.
new_home herdr-app
app_dir="$XDG_CONFIG_HOME/herdr-app"
mkdir -p "$app_dir/data" "$app_dir/layouts" "$app_dir/mods/local" "$app_dir/mods/panopticon"
app_source="$fixture/omarchy/herdr-app/.config/herdr-app"
mkdir -p "$app_source/layouts" "$app_source/mods/panopticon"
printf '// legacy Stow main\n' >"$app_source/mods/panopticon/main.js"
printf '{"title":"Panopticon","place":"none"}\n' >"$app_source/mods/panopticon/mod.json"
ln -s "$app_source/mods/panopticon/main.js" "$app_dir/mods/panopticon/main.js"
ln -s "$app_source/mods/panopticon/mod.json" "$app_dir/mods/panopticon/mod.json"
printf 'generated grid\n' >"$app_source/layouts/panopticon.json"
printf 'local grid\n' >"$app_dir/layouts/panopticon.json"
printf '{"global":{"f1":"app.help"}}\n' >"$app_dir/keys.json"
printf '{"zoom":1.5}\n' >"$app_dir/settings.json"
for file in shared.json view.json window.json data/session.json layouts/main.json \
  mods/local/main.js AGENTS.md herdr.d.ts; do
  printf 'local Herdr app state\n' >"$app_dir/$file"
done
: >"$app_dir/herdr-app.sock"
source_before="$(snapshot "$fixture")"
before="$(snapshot "$HOME")"
: >"$PANOPTICON_TEST_CURL_LOG"
: >"$PANOPTICON_TEST_RELEASE_LOG"
succeeds --only herdr-app --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Herdr app dry-run changed HOME'
[[ ! -s "$PANOPTICON_TEST_CURL_LOG" && ! -s "$PANOPTICON_TEST_RELEASE_LOG" ]] ||
  fail 'Herdr app dry-run downloaded or installed Panopticon'
before="$(snapshot "$app_dir")"
export MOCK_FAIL_PACKAGE=herdr-app
fails 'Installation failed; restoring' --only herdr-app
unset MOCK_FAIL_PACKAGE
[[ "$(snapshot "$app_dir")" == "$before" ]] || fail 'Herdr app rollback did not restore config and state'
succeeds --only herdr-app
for dir in "$app_dir" "$app_dir/mods" "$app_dir/mods/panopticon"; do
  [[ -d "$dir" && ! -L "$dir" ]] || fail "folded Herdr app runtime directory: $dir"
done
for file in keys.json settings.json; do
  [[ -L "$app_dir/$file" && "$(realpath "$app_dir/$file")" == "$fixture/omarchy/herdr-app/.config/herdr-app/$file" ]] ||
    fail "Herdr app did not link the tracked $file"
done
for file in main.js mod.json; do
  [[ -f "$app_dir/mods/panopticon/$file" && ! -L "$app_dir/mods/panopticon/$file" ]] ||
    fail "Panopticon release did not install real $file"
done
[[ -f "$app_dir/mods/panopticon/.herdr-panopticon" ]] || fail 'Panopticon release marker is missing'
grep -q 'herdr-panopticon installer' "$app_dir/mods/panopticon/.herdr-panopticon"
[[ ! -e "$app_dir/mods/panopticon/jsconfig.json" && ! -e "$app_dir/mods/panopticon/gpui-kit.d.ts" ]] ||
  fail 'installed generated Panopticon editor support'
grep -F -q 'https://raw.githubusercontent.com/eapolinario/herdr-panopticon/v0.3.0/install.sh' \
  "$PANOPTICON_TEST_CURL_LOG"
grep -q -F 'v0.3.0' "$PANOPTICON_TEST_RELEASE_LOG"
grep -q -F "$app_dir" "$PANOPTICON_TEST_RELEASE_LOG"
[[ ! -L "$app_dir/layouts/panopticon.json" ]] || fail 'stowed a runtime grid of terminal addresses'
grep -qx 'local grid' "$app_dir/layouts/panopticon.json"
grep -q '// legacy Stow main' "$XDG_CONFIG_HOME"/herdr-app.backup.*/mods/panopticon/main.js
grep -q '"f1":"app.help"' "$XDG_CONFIG_HOME"/herdr-app.backup.*/keys.json
grep -q '"zoom":1.5' "$XDG_CONFIG_HOME"/herdr-app.backup.*/settings.json
for file in shared.json view.json window.json data/session.json layouts/main.json \
  mods/local/main.js AGENTS.md herdr.d.ts; do
  [[ ! -L "$app_dir/$file" ]] || fail "linked Herdr app runtime path: $file"
  grep -q 'local Herdr app state' "$app_dir/$file"
done
[[ -e "$app_dir/herdr-app.sock" && ! -L "$app_dir/herdr-app.sock" ]] || fail 'changed Herdr app socket'
[[ ! -e "$app_dir/.gitignore" ]] || fail 'installed the Herdr app Git ignore file'
[[ ! -e "$HOME/.config" && ! -e "$SYSTEMCTL_MARKER" ]] || fail 'Herdr app ignored XDG or called systemctl'
before="$(snapshot "$HOME")"
mkdir -p "$TEST_DIR/panopticon-tmp"
export TMPDIR="$TEST_DIR/panopticon-tmp"
succeeds --only herdr-app
[[ -z "$(find "$TMPDIR" -name 'dotfiles-panopticon-installer.*' -print -quit)" ]] ||
  fail 'successful Panopticon install left its temporary installer behind'
succeeds --only herdr-app --dry-run
unset TMPDIR
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Herdr app install was not idempotent'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'Herdr app install changed source'

export PANOPTICON_TEST_RELEASE_FAIL=1
mkdir -p "$TEST_DIR/panopticon-failure-tmp"
export TMPDIR="$TEST_DIR/panopticon-failure-tmp"
before="$(snapshot "$HOME")"
fails 'Could not install Panopticon v0.3.0' --only herdr-app
unset PANOPTICON_TEST_RELEASE_FAIL TMPDIR
[[ -z "$(find "$TEST_DIR/panopticon-failure-tmp" -name 'dotfiles-panopticon-installer.*' -print -quit)" ]] ||
  fail 'failed Panopticon install left its temporary installer behind'
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Panopticon release failure was not transactional'

new_home panopticon-stow-verification
mkdir -p "$XDG_CONFIG_HOME/herdr-app"
: >"$PANOPTICON_TEST_CURL_LOG"
: >"$PANOPTICON_TEST_RELEASE_LOG"
before="$(snapshot "$HOME")"
export MOCK_SKIP_STOW=1
fails 'Stow did not install' --only herdr-app
unset MOCK_SKIP_STOW
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Panopticon Stow verification failure changed HOME'
[[ ! -s "$PANOPTICON_TEST_CURL_LOG" && ! -s "$PANOPTICON_TEST_RELEASE_LOG" ]] ||
  fail 'Panopticon release ran after incomplete Stow application'

new_home panopticon-symlink-mods
mkdir -p "$XDG_CONFIG_HOME/herdr-app" "$TEST_DIR/panopticon-outside"
ln -s "$TEST_DIR/panopticon-outside" "$XDG_CONFIG_HOME/herdr-app/mods"
before="$(snapshot "$HOME")"
fails 'Refusing Panopticon destination outside' --only herdr-app --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Panopticon mods symlink dry-run changed HOME'
fails 'Refusing Panopticon destination outside' --only herdr-app
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Panopticon mods symlink install changed HOME'

new_home panopticon-symlink-directory
mkdir -p "$XDG_CONFIG_HOME/herdr-app/mods" "$TEST_DIR/panopticon-outside"
ln -s "$TEST_DIR/panopticon-outside" "$XDG_CONFIG_HOME/herdr-app/mods/panopticon"
before="$(snapshot "$HOME")"
fails 'Refusing Panopticon destination outside' --only herdr-app --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Panopticon directory symlink dry-run changed HOME'
fails 'Refusing Panopticon destination outside' --only herdr-app
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Panopticon directory symlink install changed HOME'

new_home herdr-app-unrelated
mkdir -p "$XDG_CONFIG_HOME/herdr-app"
printf '{"global":{"f1":"app.help"}}\n' >"$XDG_CONFIG_HOME/herdr-app/keys.json"
ln -s "$TEST_DIR/unrelated.json" "$XDG_CONFIG_HOME/herdr-app/settings.json"
before="$(snapshot "$HOME")"
fails 'Refusing to replace unrelated symlink' --only herdr-app
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'foreign Herdr app config conflict changed HOME'

new_home panopticon-unmanaged
mkdir -p "$XDG_CONFIG_HOME/herdr-app/mods/panopticon"
printf '// local manual Panopticon\n' >"$XDG_CONFIG_HOME/herdr-app/mods/panopticon/main.js"
printf '{"title":"Local","place":"none"}\n' >"$XDG_CONFIG_HOME/herdr-app/mods/panopticon/mod.json"
before="$(snapshot "$HOME")"
fails 'Could not install Panopticon v0.3.0' --only herdr-app
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'unmanaged Panopticon install changed local files'
grep -q 'local manual Panopticon' "$XDG_CONFIG_HOME/herdr-app/mods/panopticon/main.js"

# New skills are discovered without editing static aliases or a second manifest.
mkdir -p "$fixture/common/skills/later-skill"
printf '%s\n' '---' 'name: later-skill' 'description: Fixture' '---' >"$fixture/common/skills/later-skill/SKILL.md"
succeeds --only copilot,claude,pi
for agent_dir in "$HOME/.copilot" "$HOME/.claude" "$HOME/.pi/agent"; do
  [[ -L "$agent_dir/skills/later-skill" ]] || fail 'new shared skill was not discovered'
done

# Pi settings use the platform override without changing local state or shared sources.
new_home pi-settings
mkdir -p "$HOME/.pi/agent/sessions"
printf '{"theme":"local-fixture"}\n' >"$HOME/.pi/agent/settings.json"
printf '{"fixture":true}\n' >"$HOME/.pi/agent/auth.json"
printf 'local session fixture\n' >"$HOME/.pi/agent/sessions/local"
source_before="$(snapshot "$fixture")"
before="$(snapshot "$HOME")"
succeeds --only pi --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Pi settings dry-run changed HOME'
succeeds --only pi
[[ "$(realpath "$HOME/.pi/agent/settings.json")" == "$fixture/omarchy/pi/.pi/agent/settings.json" ]] ||
  fail 'Pi-only install did not use the Omarchy settings'
grep -q 'local-fixture' "$HOME"/.pi/agent.backup.*/settings.json
grep -q '"fixture":true' "$HOME/.pi/agent/auth.json"
grep -q 'local session fixture' "$HOME/.pi/agent/sessions/local"
before="$(snapshot "$HOME")"
succeeds --only pi
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'Pi-only install was not idempotent'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'Pi install changed source'

# Migrate only the known legacy settings link; preserve it if Stow fails.
new_home pi-legacy
mkdir -p "$HOME/.pi/agent"
ln -s "$fixture/common/pi/.pi/agent/settings.json" "$HOME/.pi/agent/settings.json"
before="$(snapshot "$HOME")"
succeeds --only pi --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'legacy Pi settings preview changed HOME'
before="$(snapshot "$HOME/.pi/agent")"
export MOCK_FAIL_PACKAGE=agent
fails 'Installation failed; restoring' --only pi
unset MOCK_FAIL_PACKAGE
[[ "$(snapshot "$HOME/.pi/agent")" == "$before" ]] || fail 'rollback did not restore legacy Pi settings'
succeeds --only pi
[[ "$(realpath "$HOME/.pi/agent/settings.json")" == "$fixture/omarchy/pi/.pi/agent/settings.json" ]] ||
  fail 'legacy Pi settings link was not migrated'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'Pi migration changed source'

new_home pi-unrelated
mkdir -p "$HOME/.pi/agent"
ln -s "$TEST_DIR/unrelated.json" "$HOME/.pi/agent/settings.json"
before="$(snapshot "$HOME")"
fails 'Refusing to replace unrelated symlink' --only pi
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'foreign Pi settings conflict changed HOME'

# Explicit secret installation is independent of services and does not print contents.
printf 'machine example.invalid login fixture password not-a-real-credential\n' >"$fixture/common/authinfo/.authinfo"
succeeds --only authinfo --require-secrets
[[ -L "$HOME/.authinfo" ]] || fail 'did not link unlocked authinfo'
if grep -q 'not-a-real-credential' "$TEST_DIR/install.log"; then
  fail 'printed credential contents'
fi
printf '\0GITCRYPT\0fixture ciphertext\n' >"$fixture/common/authinfo/.authinfo"

# Older Stow installations fold static subdirectories such as Doom templates.
new_home folded
mkdir -p "$XDG_CONFIG_HOME/doom"
"$REAL_STOW" -d "$fixture/common" -t "$XDG_CONFIG_HOME/doom" doom
[[ -L "$XDG_CONFIG_HOME/doom/org-templates" ]] || fail 'fixture did not reproduce legacy folding'
source_before="$(snapshot "$fixture")"
before="$(snapshot "$HOME")"
succeeds --only doom --dry-run
[[ "$(snapshot "$HOME")" == "$before" ]] || fail 'folded-directory preview changed HOME'
succeeds --only doom
[[ -d "$XDG_CONFIG_HOME/doom/org-templates" && ! -L "$XDG_CONFIG_HOME/doom/org-templates" ]] ||
  fail 'legacy static directory was not unfolded'
[[ -L "$XDG_CONFIG_HOME/doom/org-templates/todo.template" ]] || fail 'unfolded templates were not linked'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'unfolding changed the repository'
new_home folded-failure
mkdir -p "$XDG_CONFIG_HOME/doom"
"$REAL_STOW" -d "$fixture/common" -t "$XDG_CONFIG_HOME/doom" doom
before="$(snapshot "$XDG_CONFIG_HOME/doom")"
export MOCK_FAIL_PACKAGE=doom
fails 'Installation failed; restoring' --only doom
unset MOCK_FAIL_PACKAGE
[[ "$(snapshot "$XDG_CONFIG_HOME/doom")" == "$before" ]] || fail 'rollback did not restore directory folding'
[[ "$(snapshot "$fixture")" == "$source_before" ]] || fail 'failed unfolding changed the repository'

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
mkdir -p "$HOME/Downloads[12]"
ln -s "$HOME/Downloads[12]" "$HOME/innocent-downloads"
fails 'glob characters' --only downloads-cleanup --downloads-dir "$HOME/innocent-downloads"
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
cleanup_backup="$(find "$XDG_CONFIG_HOME/dotfiles" -maxdepth 1 -type d -name 'downloads-cleanup.backup.*')"
[[ -d "$cleanup_backup" ]] || fail 'cleanup backup is not in its documented component directory'
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
for component in ghostty herdr-app; do
  if PATH="$TEST_DIR/doctor-bin" /usr/bin/bash "$installer" --only "$component" --doctor >"$TEST_DIR/doctor.log" 2>&1; then
    fail "doctor did not fail for missing $component"
  fi
  grep -q "\\[MISSING\\] $component" "$TEST_DIR/doctor.log"
done
[[ ! -e "$HOME" && ! -e "$SYSTEMCTL_MARKER" ]] || fail 'doctor mutated the environment'

printf 'Omarchy installer tests passed\n'
