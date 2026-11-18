#!/usr/bin/env bash
set -euo pipefail

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
wrapper="$repo_dir/common/claude/.claude/hooks/caveman-statusline.sh"
test_dir="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-claude-statusline.XXXXXXXX")"
trap 'rm -rf -- "$test_dir"' EXIT

export LC_ALL=C
export HOME="$test_dir/Home with spaces and 'quotes\" [*]"
export XDG_CONFIG_HOME="$test_dir/XDG config"
export CLAUDE_CONFIG_DIR="$test_dir/claude"
cache_dir="$CLAUDE_CONFIG_DIR/plugins/cache/caveman/caveman"
mkdir -p -- "$HOME" "$XDG_CONFIG_HOME"

add_hook() {
  local version="$1" hook
  hook="$cache_dir/$version/hooks/caveman-statusline.sh"
  mkdir -p -- "${hook%/*}"
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" %q\n' "$version" >"$hook"
}

reset_cache() {
  rm -rf -- "$cache_dir"
}

case_count=0
run_case() {
  local name="$1" expected="$2" expected_status="${3:-0}" expected_error="${4:-}" status
  if bash "$wrapper" >"$test_dir/output" 2>"$test_dir/error" <<<'statusline input'; then
    status=0
  else
    status=$?
  fi
  if [[ "$status" != "$expected_status" || "$(<"$test_dir/output")" != "$expected" ||
  "$(<"$test_dir/error")" != "$expected_error" ]]; then
    printf 'FAIL: %s\nExpected status/output/error: %s / %s / %s\nActual: %s / %s / %s\n' \
      "$name" "$expected_status" "$expected" "$expected_error" "$status" \
      "$(<"$test_dir/output")" "$(<"$test_dir/error")" >&2
    exit 1
  fi
  case_count=$((case_count + 1))
  printf 'ok %s - %s\n' "$case_count" "$name"
}

run_case 'missing cache is a silent no-op' ''
mkdir -p -- "$cache_dir"
run_case 'empty cache is a silent no-op' ''
mkdir -p -- "$cache_dir/0.10/hooks/caveman-statusline.sh" "$cache_dir/0.9/hooks"
ln -s -- "$test_dir/missing-hook" "$cache_dir/0.9/hooks/caveman-statusline.sh"
run_case 'directories and dangling hooks are not valid hooks' ''

reset_cache
add_hook 0.10
add_hook 0.9
run_case 'multi-digit minor wins regardless of creation order' 0.10
mkdir -p -- "$cache_dir/99.0/hooks"
run_case 'newer cache without a hook does not hide a valid version' 0.10

reset_cache
add_hook 0.9.9
add_hook 0.9.10
run_case 'multi-digit patch wins' 0.9.10

reset_cache
add_hook 9.99.99
add_hook 10.0.0
run_case 'major version takes precedence over minor and patch' 10.0.0

reset_cache
add_hook 0.08.009
add_hook 0.08.010
run_case 'leading zeroes do not imply octal' 0.08.010

reset_cache
add_hook 0.99999999999999999999
add_hook 0.100000000000000000000
run_case 'numeric components do not overflow shell integers' 0.100000000000000000000

reset_cache
add_hook 1.0.0
add_hook 1.0
run_case 'numerically equal versions use the first C-locale hook path' 1.0.0

reset_cache
nonnumeric_version="z cache with spaces and 'quotes\" [*]"
add_hook "$nonnumeric_version"
add_hook alpha
run_case 'nonnumeric-only caches keep a deterministic lexical fallback' "$nonnumeric_version"
mkdir -p -- "$cache_dir/999.0/hooks"
run_case 'numeric directory without a hook still permits the fallback' "$nonnumeric_version"
add_hook 0.1
run_case 'numeric versions take precedence over nonnumeric cache names' 0.1

cat >"$cache_dir/0.1/hooks/caveman-statusline.sh" <<'HOOK'
#!/usr/bin/env bash
IFS= read -r payload
printf '%s (%s)\n' "$payload" "$LC_ALL"
printf 'hook diagnostic\n' >&2
exit 7
HOOK
LC_ALL=POSIX run_case 'delegation preserves stdin, locale, stderr and exit status' \
  'statusline input (POSIX)' 7 'hook diagnostic'

export CLAUDE_CONFIG_DIR="$test_dir/Claude config with spaces and 'quotes\" [*]"$'\nsecond line'
cache_dir="$CLAUDE_CONFIG_DIR/plugins/cache/caveman/caveman"
add_hook 0.10
add_hook 0.9
run_case 'config paths preserve spaces, quotes, glob characters and newlines' 0.10

unset CLAUDE_CONFIG_DIR
cache_dir="$HOME/.claude/plugins/cache/caveman/caveman"
add_hook 0.10
add_hook 0.9
run_case 'unset config uses the quoted HOME fallback' 0.10
export CLAUDE_CONFIG_DIR=''
run_case 'empty config also uses the HOME fallback' 0.10

printf 'Passed %s Claude statusline regression cases.\n' "$case_count"
