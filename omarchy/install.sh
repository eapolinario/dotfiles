#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMMON_DIR="$(realpath "$SCRIPT_DIR/../common")"
readonly SCRIPT_DIR COMMON_DIR
readonly -a DEFAULT_COMPONENTS=(doom authinfo hypr ghostty nvim copilot claude pi grasp)
# Apply these exclusions to both the file plan and Stow, including local runtime files.
readonly STOW_IGNORE='(^|/)(README[^/]*|LICENSE[^/]*|COPYING|tests|\.git|\.gitignore|\.stow-local-ignore|\.DS_Store)(/|$)|~$'

dry_run=false
doctor=false
enable_services=false
require_secrets=false
with_downloads_cleanup=false
downloads_age=7d
downloads_dir=
cleanup_options=false
selection=
config_home=
transaction_active=false
links_started=false
declare -a components=() services=()
declare -a package_dirs=() package_names=() package_targets=() package_ignores=()
declare -a plan_kinds=() plan_sources=() plan_targets=() plan_components=() plan_relatives=()
declare -a plan_states=() plan_backups=() created_dirs=() temporary_files=()
declare -A seen_targets=() backup_prefixes=() backup_dirs=() component_roots=()

usage() {
  printf '%s\n' \
    'Usage: install.sh [OPTIONS]' \
    '' \
    '  -n, --dry-run                 Show the complete file/service plan; write nothing' \
    '      --doctor                  Report selected component prerequisites; write nothing' \
    '      --only COMPONENTS         Comma-separated component selection (see below)' \
    '      --nvim-only               Alias for --only nvim; never enables services' \
    '      --with-downloads-cleanup  Opt in to the private Downloads cleanup rule and unit' \
    '      --downloads-dir PATH      Cleanup directory (default: $HOME/Downloads)' \
    '      --downloads-age AGE       Retention: 7d by default; 0 explicitly deletes all contents' \
    '                                Units: s, min, h, d, w (for example 30d)' \
    '      --enable-services         Enable/start selected services after installing files' \
    '      --require-secrets         Fail rather than skip missing/locked authinfo' \
    '  -h, --help                    Show this help' \
    '' \
    'Default components: doom,authinfo,hypr,ghostty,nvim,copilot,claude,pi,grasp' \
    'Optional component: downloads-cleanup' \
    'Services are NOT enabled or started by default. Cleanup is never selected implicitly.'
}

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

warn() {
  printf 'WARNING: %s\n' "$*" >&2
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || fail "Missing dependency: $1. Install it via your package manager."
}

files_equal() {
  local status
  if cmp -s "$@"; then
    return 0
  else
    status=$?
  fi
  [[ "$status" == 1 ]] || fail 'Could not read files while planning or restoring installation.'
  return 1
}

selected() {
  local component
  for component in "${components[@]}"; do
    [[ "$component" != "$1" ]] || return 0
  done
  return 1
}

parse_args() {
  local nvim_only=false
  while (($#)); do
    case "$1" in
      -n | --dry-run) dry_run=true ;;
      --doctor) doctor=true ;;
      --only | --downloads-dir | --downloads-age)
        (($# >= 2)) && [[ -n "$2" && "$2" != --* ]] || fail "$1 requires a value."
        case "$1" in
          --only)
            [[ -z "$selection" ]] || fail 'Specify component selection only once.'
            selection="$2"
            ;;
          --downloads-dir)
            downloads_dir="$2"
            cleanup_options=true
            ;;
          --downloads-age)
            downloads_age="$2"
            cleanup_options=true
            ;;
        esac
        shift
        ;;
      --nvim-only)
        [[ -z "$selection" ]] || fail 'Specify component selection only once.'
        selection=nvim
        nvim_only=true
        ;;
      --with-downloads-cleanup) with_downloads_cleanup=true ;;
      --enable-services) enable_services=true ;;
      --require-secrets) require_secrets=true ;;
      -h | --help)
        usage
        exit 0
        ;;
      *) fail "Unknown option: $1 (see --help)." ;;
    esac
    shift
  done
  if [[ "$nvim_only" == true && ("$enable_services" == true || "$with_downloads_cleanup" == true) ]]; then
    fail '--nvim-only cannot be combined with service options.'
  fi
  if [[ -n "$selection" ]]; then
    [[ "$selection" != ,* && "$selection" != *, && "$selection" != *,,* ]] ||
      fail 'Component selection must not contain empty entries.'
    IFS=, read -r -a components <<<"$selection"
  else
    components=("${DEFAULT_COMPONENTS[@]}")
  fi
  if [[ "$with_downloads_cleanup" == true ]] && ! selected downloads-cleanup; then
    components+=(downloads-cleanup)
  fi
  local component
  local -A seen=()
  for component in "${components[@]}"; do
    case "$component" in
      doom | authinfo | hypr | ghostty | nvim | copilot | claude | pi | grasp | downloads-cleanup) ;;
      *) fail "Unknown component: $component (see --help)." ;;
    esac
    [[ -z "${seen[$component]:-}" ]] || fail "Duplicate component: $component."
    seen["$component"]=1
  done
  if [[ "$cleanup_options" == true ]] && ! selected downloads-cleanup; then
    fail 'Cleanup options require --with-downloads-cleanup or --only downloads-cleanup.'
  fi
  if [[ "$require_secrets" == true ]] && ! selected authinfo; then
    fail '--require-secrets requires selecting authinfo.'
  fi
  if [[ "$enable_services" == true ]] && ! selected grasp && ! selected downloads-cleanup; then
    fail '--enable-services requires selecting grasp or downloads-cleanup.'
  fi
}

authinfo_available() {
  local source="$COMMON_DIR/authinfo/.authinfo"
  if [[ ! -f "$source" || ! -r "$source" ]]; then
    warn 'Authinfo is missing or unreadable; leaving existing credentials untouched.'
    return 1
  fi
  # Inspect only git-crypt's fixed header, never credentials or git-crypt key files.
  if files_equal -n 10 -- "$source" <(printf '\0GITCRYPT\0'); then
    warn 'Authinfo is locked; unlock the repository with git-crypt before installing credentials.'
    return 1
  fi
}

check_nvim_support() {
  if [[ -d "${OMARCHY_PATH:-$HOME/.local/share/omarchy}" &&
    ! -r "$config_home/nvim/lua/config/remote_clipboard.lua" ]]; then
    fail "Omarchy Neovim support files are missing from $config_home/nvim. Run omarchy-nvim-setup first."
  fi
}

doctor_cmd() {
  if command -v "$1" >/dev/null 2>&1; then
    printf '[OK] %s: %s\n' "$1" "$2"
  else
    printf '[MISSING] %s: %s\n' "$1" "$2"
    doctor_status=1
  fi
}

run_doctor() {
  local doctor_status=0 component
  doctor_cmd stow 'linking selected configurations'
  for component in "${components[@]}"; do
    printf '\n%s\n' "$component"
    case "$component" in
      doom)
        doctor_cmd emacs 'Doom editor'
        if [[ -x "$config_home/emacs/bin/doom" || -x "$HOME/.emacs.d/bin/doom" ]] ||
          command -v doom >/dev/null 2>&1; then
          printf '[OK] Doom bootstrap found; use doom sync after changing modules/packages.\n'
        else
          printf '[MISSING] Doom bootstrap; this installer installs config, not Doom itself.\n'
          doctor_status=1
        fi
        ;;
      authinfo)
        if authinfo_available; then
          printf '[OK] Unlocked authinfo is available (contents not displayed).\n'
        elif [[ "$require_secrets" == true ]]; then
          doctor_status=1
        fi
        ;;
      hypr)
        doctor_cmd hyprctl 'Hyprland Lua session'
        doctor_cmd jq 'workspace JSON snapshots'
        doctor_cmd zenity 'workspace destination prompt'
        ;;
      ghostty) doctor_cmd ghostty 'terminal' ;;
      nvim)
        doctor_cmd nvim 'shared LazyVim configuration'
        doctor_cmd git 'plugin bootstrap'
        if [[ -d "${OMARCHY_PATH:-$HOME/.local/share/omarchy}" &&
          ! -r "$config_home/nvim/lua/config/remote_clipboard.lua" ]]; then
          printf '[MISSING] Omarchy clipboard support; run omarchy-nvim-setup first.\n'
          doctor_status=1
        fi
        ;;
      copilot | claude | pi)
        doctor_cmd "$component" 'agent CLI (not installed by this script)'
        [[ "$component" != claude ]] || doctor_cmd rtk 'configured Claude command hook'
        printf '[INFO] Shared skills are discovered from %s/skills/*/SKILL.md.\n' "$COMMON_DIR"
        ;;
      grasp)
        doctor_cmd systemctl 'optional user-service activation'
        doctor_cmd uvx 'Grasp backend; first launch can download its package'
        if [[ ! -d "$HOME/org" ]]; then
          printf '[MISSING] %s/org: create your capture directory before starting Grasp.\n' "$HOME"
          doctor_status=1
        fi
        ;;
      downloads-cleanup)
        doctor_cmd systemctl 'optional user-service activation'
        doctor_cmd systemd-tmpfiles 'explicit, private cleanup rule'
        printf '[WARNING] Cleanup retention %s in %s; deletion does not use Trash.\n' \
          "$downloads_age" "$downloads_dir"
        ;;
    esac
  done
  return "$doctor_status"
}

validate_cleanup_characters() {
  [[ ! "$downloads_dir" =~ [[:cntrl:]] && "$downloads_dir" != *[\*\?\[\]\\]* ]] ||
    fail 'Cleanup directory must not contain control characters, glob characters, or backslashes.'
}

validate_cleanup() {
  [[ "$downloads_age" =~ ^([0-9]{1,8}(s|min|h|d|w)|0)$ ]] ||
    fail 'Invalid retention; use an integer with s/min/h/d/w, or explicit 0.'
  [[ "$downloads_dir" == /* ]] || fail '--downloads-dir must be an absolute path.'
  validate_cleanup_characters
  downloads_dir="$(realpath -m -- "$downloads_dir")"
  validate_cleanup_characters
  [[ "$downloads_dir" != / ]] || fail 'Refusing cleanup of the filesystem root.'
  [[ "$(realpath -m -- "$HOME")/" != "$downloads_dir/"* ]] ||
    fail 'Refusing cleanup of HOME or one of its ancestors.'
  local protected
  for protected in "$config_home" "$COMMON_DIR/.." "$HOME/.local" \
    "$HOME/.copilot" "$HOME/.claude" "$HOME/.pi"; do
    protected="$(realpath -m -- "$protected")"
    [[ "$protected/" != "$downloads_dir/"* && "$downloads_dir/" != "$protected/"* ]] ||
      fail "Refusing cleanup of $downloads_dir: it overlaps a protected config/repository path."
  done
  [[ ! -e "$downloads_dir" || -d "$downloads_dir" ]] ||
    fail "Cleanup path is not a directory: $downloads_dir."
}

plan_entry() {
  local kind="$1" source="$2" target="$3" component="$4" relative="$5"
  [[ -z "${seen_targets[$target]:-}" ]] || fail "Duplicate planned target: $target."
  [[ ! "$target" =~ [[:cntrl:]] && ! "$relative" =~ [[:cntrl:]] ]] ||
    fail 'Managed paths must not contain control characters.'
  seen_targets["$target"]=1
  plan_kinds+=("$kind")
  plan_sources+=("$source")
  plan_targets+=("$target")
  plan_components+=("$component")
  plan_relatives+=("$relative")
  plan_backups+=('')
}

add_package() {
  local component="$1" parent="$2" package="$3" target="$4" ignore="${5:-}"
  local source_root="$parent/$package" rel
  [[ -d "$source_root" ]] || fail "Missing $component configuration: $source_root."
  package_dirs+=("$parent")
  package_names+=("$package")
  package_targets+=("$target")
  package_ignores+=("$ignore")
  backup_prefixes["$component"]="$target.backup"
  component_roots["$component"]="$target"
  local -a files=()
  mapfile -d '' -t files < <(find "$source_root" -mindepth 1 \( -type f -o -type l \) -printf '%P\0' | sort -z)
  wait "$!" || fail "Could not enumerate $source_root."
  for rel in "${files[@]}"; do
    [[ ! "$rel" =~ $STOW_IGNORE ]] || continue
    [[ -z "$ignore" || ! "$rel" =~ $ignore ]] || continue
    plan_entry link "$source_root/$rel" "$target/$rel" "$component" "$rel"
  done
}

add_agent() {
  local component="$1" target="$2" parent="$3" package="$4" skill_file name
  add_package "$component" "$parent" "$package" "$target" '(^|/)skills(/|$)'
  [[ -d "$COMMON_DIR/skills" ]] || fail "Shared skill directory not found: $COMMON_DIR/skills."
  for skill_file in "$COMMON_DIR"/skills/*/SKILL.md; do
    [[ -f "$skill_file" ]] || continue
    name="$(basename "$(dirname "$skill_file")")"
    plan_entry skill "$(dirname "$skill_file")" "$target/skills/$name" "$component" "skills/$name"
  done
}

build_plan() {
  local component name path escaped
  for component in "${components[@]}"; do
    case "$component" in
      doom) add_package doom "$COMMON_DIR" doom "$config_home/doom" ;;
      authinfo)
        if authinfo_available; then
          add_package authinfo "$COMMON_DIR" authinfo "$HOME"
          backup_prefixes[authinfo]="$HOME/.authinfo.backup"
        elif [[ "$require_secrets" == true ]]; then
          fail 'Required authinfo is unavailable; no files changed.'
        else
          printf 'Skipping authinfo; other selected configurations can still be installed.\n'
        fi
        ;;
      hypr)
        add_package hypr "$SCRIPT_DIR/hypr/.config" hypr "$config_home/hypr"
        for name in input.conf bindings.conf looknfeel.conf monitors.conf; do
          path="$config_home/hypr/$name"
          if [[ -L "$path" && "$(realpath -m "$path")" == "$SCRIPT_DIR/hypr/.config/hypr/$name" ]]; then
            plan_entry remove '' "$path" hypr "$name"
          fi
        done
        ;;
      ghostty) add_package ghostty "$SCRIPT_DIR/ghostty/.config" ghostty "$config_home/ghostty" ;;
      nvim)
        check_nvim_support
        add_package nvim "$COMMON_DIR" nvim "$config_home/nvim" '^lazy-lock\.json$|^lua/plugins/theme\.lua$'
        ;;
      copilot) add_agent copilot "$HOME/.copilot" "$COMMON_DIR/copilot" .copilot ;;
      claude) add_agent claude "$HOME/.claude" "$COMMON_DIR/claude" .claude ;;
      pi) add_agent pi "$HOME/.pi/agent" "$COMMON_DIR/pi/.pi" agent ;;
      grasp)
        add_package grasp "$SCRIPT_DIR/systemd/.config/systemd" user "$config_home/systemd/user" \
          '^downloads-clean-at-login\.service$'
        backup_prefixes[grasp]="$config_home/systemd/grasp.backup"
        services+=(grasp.service)
        ;;
      downloads-cleanup)
        add_package downloads-cleanup "$SCRIPT_DIR/systemd/.config/systemd" user "$config_home/systemd/user" \
          '^grasp\.service$'
        backup_prefixes["downloads-cleanup"]="$config_home/dotfiles/downloads-cleanup.backup"
        # A private file, outside user-tmpfiles.d, cannot be picked up by general cleanup.
        escaped="${downloads_dir//%/%%}"
        escaped="${escaped//\"/\\\"}"
        plan_entry write "e \"$escaped\" - - - $downloads_age" \
          "$config_home/dotfiles/downloads-cleanup.conf" downloads-cleanup downloads-cleanup.conf
        services+=(downloads-clean-at-login.service)
        warn "Opted-in cleanup removes contents older than $downloads_age from $downloads_dir, without Trash."
        ;;
    esac
  done
}

preflight() {
  local i target parent source source_parent component resolved fold_target fold_source
  for i in "${!plan_targets[@]}"; do
    target="${plan_targets[$i]}"
    component="${plan_components[$i]}"
    parent="${target%/*}"
    source_parent="$(dirname "${plan_sources[$i]}")"
    fold_target=
    fold_source=
    while [[ "$parent" != / ]]; do
      if [[ -L "$parent" || (-e "$parent" && ! -d "$parent") ]]; then
        if [[ -L "$parent" && "${plan_kinds[$i]}" == link &&
          ("$component" == doom || "$component" == hypr || "$component" == ghostty) &&
          "$parent" == "${component_roots[$component]}/"* &&
          "$(realpath -m -- "$parent")" == "$(realpath -m -- "$source_parent")" ]]; then
          # Keep only the outermost folded directory: never move files through
          # a folded parent, which would modify the repository itself.
          fold_target="$parent"
          fold_source="$source_parent"
        elif [[ "$component" == nvim ]]; then
          fail "Expected a real Neovim directory at $parent; no Neovim files were changed."
        else
          fail "Expected a real directory at $parent; no files changed."
        fi
      fi
      parent="${parent%/*}"
      [[ -n "$parent" ]] || parent=/
      source_parent="$(dirname "$source_parent")"
    done
    if [[ -n "$fold_target" ]]; then
      if [[ -z "${seen_targets[$fold_target]:-}" ]]; then
        plan_entry unfold "$fold_source" "$fold_target" "$component" \
          "${fold_target#"${component_roots[$component]}/"}"
        plan_states[${#plan_targets[@]} - 1]=replace
      fi
      plan_states[$i]=create
    elif [[ "${plan_kinds[$i]}" == remove ]]; then
      plan_states[$i]=remove
    elif [[ -L "$target" ]]; then
      resolved="$(realpath -m -- "$target")"
      source="$(realpath -m -- "${plan_sources[$i]}")"
      if [[ "${plan_kinds[$i]}" != write && "$resolved" == "$source" ]]; then
        plan_states[$i]=keep
      elif [[ "$component" == nvim &&
        "$resolved" == "$SCRIPT_DIR/nvim/.config/nvim/${plan_relatives[$i]}" ]]; then
        plan_states[$i]=replace
      else
        [[ "$component" != nvim ]] || fail "Refusing to replace unrelated Neovim symlink $target -> $resolved."
        fail "Refusing to replace unrelated symlink $target -> $resolved."
      fi
    elif [[ -f "$target" ]]; then
      [[ -r "$target" ]] || fail "Cannot back up unreadable file: $target."
      if [[ "${plan_kinds[$i]}" == write ]] &&
        files_equal -- "$target" <(printf '%s\n' "${plan_sources[$i]}"); then
        plan_states[$i]=keep
      else
        plan_states[$i]=replace
      fi
    elif [[ -e "$target" ]]; then
      fail "Expected a file or managed symlink at $target; no files changed."
    else
      plan_states[$i]=create
    fi
  done
  if [[ "$enable_services" == true ]]; then
    require_cmd systemctl
    systemctl --user show-environment >/dev/null 2>&1 ||
      fail 'User systemd instance unavailable; no files changed.'
    if selected grasp; then
      require_cmd uvx
      [[ -d "$HOME/org" ]] || fail "Create $HOME/org before enabling Grasp."
    fi
    if selected downloads-cleanup; then
      require_cmd systemd-tmpfiles
    fi
  fi
}

stow_command() {
  local i="$1"
  stow_args=(stow --no-folding "--ignore=$STOW_IGNORE")
  [[ -z "${package_ignores[$i]}" ]] || stow_args+=("--ignore=${package_ignores[$i]}")
  stow_args+=(-d "${package_dirs[$i]}" -vt "${package_targets[$i]}" "${package_names[$i]}")
}

print_plan() {
  local i
  local -a stow_args=()
  for i in "${!plan_targets[@]}"; do
    [[ "${plan_states[$i]}" != keep ]] || continue
    printf '[PLAN] %s %s: %s\n' "${plan_states[$i]}" "${plan_kinds[$i]}" "${plan_targets[$i]}"
  done
  for i in "${!package_names[@]}"; do
    stow_command "$i"
    printf '[PLAN]'
    printf ' %q' "${stow_args[@]}"
    printf '\n'
  done
  if [[ "$enable_services" == true ]]; then
    printf '[PLAN] systemctl --user daemon-reload\n'
    printf '[PLAN] systemctl --user enable --now %s\n' "${services[@]}"
  else
    printf '[PLAN] No services will be enabled, started, stopped, or reloaded.\n'
  fi
}

make_dir() {
  local dir="$1"
  [[ ! -d "$dir" ]] || return 0
  make_dir "$(dirname "$dir")"
  mkdir -- "$dir"
  created_dirs+=("$dir")
}

backup_entry() {
  local i="$1" component="${plan_components[$1]}" dir backup
  if [[ -z "${backup_dirs[$component]:-}" ]]; then
    make_dir "$(dirname "${backup_prefixes[$component]}")"
    dir="$(mktemp -d "${backup_prefixes[$component]}.XXXXXXXX")"
    backup_dirs["$component"]="$dir"
    printf 'Backing up %s files to %s\n' "$component" "$dir"
  fi
  dir="${backup_dirs[$component]}"
  backup="$dir/${plan_relatives[$i]}"
  make_dir "$(dirname "$backup")"
  plan_backups[$i]="$backup"
  mv -- "${plan_targets[$i]}" "$backup"
  printf '%s\t%s\n' "${plan_targets[$i]}" "${plan_relatives[$i]}" >>"$dir/RESTORE.tsv"
}

rollback() {
  local status="$1" i target source failed=false
  trap - EXIT INT TERM
  if [[ "$transaction_active" == true && "$status" != 0 ]]; then
    printf 'Installation failed; restoring changed files from backups.\n' >&2
    for ((i = ${#plan_targets[@]} - 1; i >= 0; i--)); do
      [[ "$links_started" == true ]] || break
      [[ "${plan_states[$i]}" != keep ]] || continue
      target="${plan_targets[$i]}"
      if [[ -L "$target" && ("${plan_kinds[$i]}" == link || "${plan_kinds[$i]}" == skill) ]]; then
        source="$(realpath -m -- "${plan_sources[$i]}")"
        if [[ "$(realpath -m -- "$target")" == "$source" ]]; then
          rm -- "$target" || failed=true
        fi
      elif [[ -f "$target" && ! -L "$target" && "${plan_kinds[$i]}" == write ]]; then
        if cmp -s -- "$target" <(printf '%s\n' "${plan_sources[$i]}"); then
          rm -- "$target" || failed=true
        else
          printf 'Preserving changed/unreadable generated file during rollback: %s\n' "$target" >&2
          failed=true
        fi
      fi
    done
    for i in "${temporary_files[@]}"; do
      rm -f -- "$i" || failed=true
    done
    for ((i = ${#created_dirs[@]} - 1; i >= 0; i--)); do
      # Only remove empty directories created by this invocation, never a tree.
      if [[ -d "${created_dirs[$i]}" ]]; then
        rmdir --ignore-fail-on-non-empty -- "${created_dirs[$i]}" || failed=true
      fi
    done
    # Restore directory symlinks only after removing their newly linked children
    # and empty replacement directories.
    for ((i = ${#plan_targets[@]} - 1; i >= 0; i--)); do
      [[ -n "${plan_backups[$i]}" ]] || continue
      [[ -e "${plan_backups[$i]}" || -L "${plan_backups[$i]}" ]] || continue
      target="${plan_targets[$i]}"
      if [[ -e "$target" || -L "$target" ]]; then
        printf 'Restore blocked by changed path: %s (backup: %s)\n' "$target" "${plan_backups[$i]}" >&2
        failed=true
      else
        mv -- "${plan_backups[$i]}" "$target" || failed=true
      fi
    done
    if [[ "$failed" == true ]]; then
      printf 'Some paths need manual restoration; preserved backups and RESTORE.tsv identify them.\n' >&2
    fi
  fi
  exit "$status"
}

apply_plan() {
  local i target temporary source
  local -a stow_args=()
  transaction_active=true
  trap 'rollback "$?"' EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM
  for i in "${!plan_targets[@]}"; do
    case "${plan_states[$i]}" in
      replace | remove) backup_entry "$i" ;;
    esac
  done
  links_started=true
  # Track parent directories ourselves so even a partially failed Stow run can
  # be rolled back without leaving a fresh configuration tree behind.
  for i in "${!plan_targets[@]}"; do
    [[ "${plan_kinds[$i]}" != remove ]] || continue
    make_dir "$(dirname "${plan_targets[$i]}")"
  done
  for i in "${!package_names[@]}"; do
    make_dir "${package_targets[$i]}"
    stow_command "$i"
    "${stow_args[@]}"
  done
  for i in "${!plan_targets[@]}"; do
    [[ "${plan_states[$i]}" != keep ]] || continue
    target="${plan_targets[$i]}"
    case "${plan_kinds[$i]}" in
      skill)
        make_dir "$(dirname "$target")"
        ln -s -- "$(realpath --relative-to="$(dirname "$target")" "${plan_sources[$i]}")" "$target"
        ;;
      write)
        make_dir "$(dirname "$target")"
        temporary="$(mktemp "$target.tmp.XXXXXXXX")"
        temporary_files+=("$temporary")
        printf '%s\n' "${plan_sources[$i]}" >"$temporary"
        mv -- "$temporary" "$target"
        ;;
    esac
  done
  # Detect Stow ignore overrides or incomplete application instead of reporting success.
  for i in "${!plan_targets[@]}"; do
    case "${plan_kinds[$i]}" in
      link | skill)
        target="${plan_targets[$i]}"
        source="$(realpath -m -- "${plan_sources[$i]}")"
        [[ -L "$target" && "$(realpath -m -- "$target")" == "$source" ]] ||
          fail "Stow did not install $target; check custom Stow ignore rules."
        ;;
      unfold)
        [[ -d "${plan_targets[$i]}" && ! -L "${plan_targets[$i]}" ]] ||
          fail "Could not unfold ${plan_targets[$i]}."
        ;;
    esac
  done
  transaction_active=false
  trap - EXIT INT TERM
  printf 'Selected configurations installed. Any replaced files were backed up with RESTORE.tsv.\n'
  if [[ "$enable_services" == true ]]; then
    # Service activation is deliberately outside file rollback: cleanup cannot be undone.
    systemctl --user daemon-reload ||
      fail 'Files installed, but daemon-reload failed; service state was not rolled back.'
    for source in "${services[@]}"; do
      systemctl --user enable --now "$source" ||
        fail "Files installed, but $source activation failed. Inspect its enabled state and journal."
    done
  else
    printf 'No service state changed.\n'
  fi
}

main() {
  parse_args "$@"
  [[ "$(uname -s)" == Linux ]] || fail 'This installer is intended for Linux hosts.'
  [[ "$HOME" == /* ]] || fail 'HOME must be an absolute path.'
  [[ "${XDG_CONFIG_HOME:-$HOME/.config}" == /* ]] || fail 'XDG_CONFIG_HOME must be an absolute path.'
  config_home="$(realpath -ms -- "${XDG_CONFIG_HOME:-$HOME/.config}")"
  downloads_dir="${downloads_dir:-$HOME/Downloads}"
  if selected downloads-cleanup; then
    validate_cleanup
  fi
  if [[ "$doctor" == true ]]; then
    run_doctor
    return
  fi
  require_cmd stow
  build_plan
  preflight
  print_plan
  if [[ "$dry_run" == true ]]; then
    printf 'Dry run complete; no files or service state changed.\n'
    return
  fi
  apply_plan
}

main "$@"
