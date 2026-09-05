#!/usr/bin/env bash
set -euo pipefail

test_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
cd "$test_dir"

for tool in jq nvim; do
  command -v "$tool" >/dev/null || {
    printf 'Missing test dependency: %s\n' "$tool" >&2
    exit 1
  }
done

state_dir=".workspace-test-$$"
mkdir -- "$state_dir"
trap 'rm -rf -- "$state_dir"' EXIT

export SWAP_TEST_STATE="$test_dir/$state_dir"
export SWAP_TEST_REAL_JQ
SWAP_TEST_REAL_JQ=$(command -v jq)
clients_fixture=$("$SWAP_TEST_REAL_JQ" -c '.' fixtures/workspace-clients.json)
swap_script="$test_dir/../hypr/.config/hypr/scripts/swap-workspace.sh"
fixture="$test_dir/fixtures/workspace-command.sh"
[[ -x "$fixture" ]] || {
  printf 'Missing executable workspace fixture: %s\n' "$fixture" >&2
  exit 1
}

export HOME="$SWAP_TEST_STATE/Home with spaces and 'quotes"
export XDG_CONFIG_HOME="$SWAP_TEST_STATE/XDG config with spaces and 'quotes"
export XDG_DATA_HOME="$SWAP_TEST_STATE/data"
export XDG_STATE_HOME="$SWAP_TEST_STATE/state"
export XDG_CACHE_HOME="$SWAP_TEST_STATE/cache"
export NVIM_LOG_FILE="$SWAP_TEST_STATE/nvim.log"

mkdir -p -- "$SWAP_TEST_STATE/bin" \
  "$HOME/.config/hypr/scripts" "$XDG_CONFIG_HOME/hypr/scripts"
for command in hyprctl jq zenity; do
  ln -s -- "$fixture" "$SWAP_TEST_STATE/bin/$command"
done
for config in "$HOME/.config" "$XDG_CONFIG_HOME"; do
  ln -s -- "$fixture" "$config/hypr/scripts/swap-workspace.sh"
done
export PATH="$SWAP_TEST_STATE/bin:$PATH"

assert_equal() {
  if [[ "$1" != "$2" ]]; then
    printf 'Expected:\n%s\nActual:\n%s\n' "$1" "$2" >&2
    exit 1
  fi
}

reset_case() {
  export SWAP_TEST_ACTIVE='{"id":1}'
  export SWAP_TEST_DEST=2
  export SWAP_TEST_CLIENTS="$clients_fixture"
  export SWAP_TEST_HYPRCTL_FAILURE=''
  export SWAP_TEST_JQ_FAILURE_AT=''
  export SWAP_TEST_ZENITY_STATUS=0
  : >"$SWAP_TEST_STATE/calls"
  : >"$SWAP_TEST_STATE/clients-read"
  : >"$SWAP_TEST_STATE/dispatches"
  printf '0\n' >"$SWAP_TEST_STATE/jq-calls"
}

expected_dispatches() {
  printf 'hl.dsp.focus({ workspace = "%s" })\n' "$1"
  shift
  while (($#)); do
    printf 'hl.dsp.window.move({ workspace = "%s", follow = false, window = "address:%s" })\n' "$1" "$2"
    shift 2
  done
}

case_count=0
run_case() {
  local name="$1" expected_status="$2" error_fragment="$3" dispatches="${4:-}" status
  if "$swap_script" >"$SWAP_TEST_STATE/output" 2>"$SWAP_TEST_STATE/error"; then
    status=0
  else
    status=$?
  fi
  assert_equal "$expected_status" "$status"
  assert_equal "$dispatches" "$(<"$SWAP_TEST_STATE/dispatches")"
  if [[ -n "$error_fragment" ]]; then
    if [[ "$(<"$SWAP_TEST_STATE/error")" != *"$error_fragment"* ]]; then
      printf 'Missing error "%s" in:\n%s\n' "$error_fragment" "$(<"$SWAP_TEST_STATE/error")" >&2
      exit 1
    fi
  else
    assert_equal '' "$(<"$SWAP_TEST_STATE/error")"
  fi
  if [[ "$expected_status" == 0 && -n "$dispatches" ]]; then
    assert_equal '1' "$(<"$SWAP_TEST_STATE/clients-read")"
  fi
  case_count=$((case_count + 1))
  printf 'ok %s - %s\n' "$case_count" "$name"
}

reset_case
run_case 'occupied workspace 99 stays untouched' 0 '' \
  "$(expected_dispatches 2 2 0x101 1 0x201 2 0x102)"

reset_case
SWAP_TEST_DEST=99
run_case 'workspace 99 is a valid destination' 0 '' \
  "$(expected_dispatches 99 99 0x101 1 0x9901 99 0x102)"

reset_case
SWAP_TEST_ACTIVE='{"id":99}'
SWAP_TEST_DEST=1
run_case 'workspace 99 is a valid source' 0 '' \
  "$(expected_dispatches 1 99 0x101 1 0x9901 99 0x102)"

reset_case
SWAP_TEST_DEST=4
run_case 'empty destination workspace' 0 '' "$(expected_dispatches 4 4 0x101 4 0x102)"

reset_case
SWAP_TEST_ACTIVE='{"id":8}'
run_case 'empty source workspace' 0 '' "$(expected_dispatches 2 8 0x201)"

reset_case
SWAP_TEST_CLIENTS='[]'
run_case 'no windows on either workspace' 0 '' "$(expected_dispatches 2)"

reset_case
SWAP_TEST_DEST=0002
run_case 'leading zeroes normalize to decimal' 0 '' \
  "$(expected_dispatches 2 2 0x101 1 0x201 2 0x102)"

reset_case
SWAP_TEST_DEST=08
run_case 'leading zeroes do not imply octal' 0 '' "$(expected_dispatches 8 8 0x101 8 0x102)"

reset_case
SWAP_TEST_DEST=0001
run_case 'normalized current workspace is a no-op' 0 ''
assert_equal '' "$(<"$SWAP_TEST_STATE/clients-read")"

reset_case
SWAP_TEST_DEST=2147483647
run_case 'largest supported workspace ID' 0 '' \
  "$(expected_dispatches 2147483647 2147483647 0x101 2147483647 0x102)"

for dest in '' 0 000 -1 abc '2;exit' ' 2' '2 ' 2.5 special:scratch; do
  reset_case
  SWAP_TEST_DEST="$dest"
  run_case "invalid input: '$dest'" 1 'Destination must be a positive workspace ID.'
done

for dest in 2147483648 123456789012345678901234567890; do
  reset_case
  SWAP_TEST_DEST="$dest"
  run_case "out-of-range input: $dest" 1 'Destination workspace ID must not exceed'
done

reset_case
SWAP_TEST_DEST=''
SWAP_TEST_ZENITY_STATUS=1
run_case 'cancel is a clean no-op' 0 ''
assert_equal '' "$(<"$SWAP_TEST_STATE/clients-read")"

reset_case
SWAP_TEST_ZENITY_STATUS=5
run_case 'zenity failure is not cancellation' 1 'Unable to prompt for a workspace'

for active in '' 'not json' '{}' '{"id":null}' '{"id":"1"}' '{"id":-1}' '{"id":0}' \
  '{"id":1.5}' '{"id":2147483648}' '{"id":1} {"id":2}'; do
  reset_case
  SWAP_TEST_ACTIVE="$active"
  run_case "invalid active workspace: $active" 1 'Unable to parse a positive active workspace ID.'
done

reset_case
SWAP_TEST_HYPRCTL_FAILURE=activeworkspace
run_case 'hyprctl active workspace failure' 1 'Unable to query the active workspace.'

reset_case
SWAP_TEST_HYPRCTL_FAILURE=clients
run_case 'hyprctl clients failure' 1 'Unable to query workspace windows.'

reset_case
SWAP_TEST_JQ_FAILURE_AT=1
run_case 'jq active workspace failure' 1 'Unable to parse a positive active workspace ID.'

reset_case
SWAP_TEST_JQ_FAILURE_AT=2
run_case 'jq clients failure is not hidden by process substitution' 1 'Unable to parse workspace windows.'

for clients in '' ' ' '[] []' 'not json' '{}' '[{"workspace":{"id":1}}]' \
  '[{"workspace":{"id":1},"address":"invalid"}]' \
  '[{"workspace":{"id":1},"address":"0x1\"; error()"}]'; do
  reset_case
  SWAP_TEST_CLIENTS="$clients"
  run_case "invalid clients: $clients" 1 'Unable to parse workspace windows.'
done

reset_case
SWAP_TEST_HYPRCTL_FAILURE=focus
run_case 'focus failure stops before moving windows' 1 'Unable to focus workspace 2.' \
  "$(expected_dispatches 2)"

reset_case
SWAP_TEST_HYPRCTL_FAILURE=move
run_case 'move failure stops further dispatches' 1 'Unable to move window 0x101 to workspace 2.' \
  "$(expected_dispatches 2 2 0x101)"

printf 'Passed %s workspace regression cases.\n' "$case_count"
nvim --headless -u NONE -i NONE -n -l "$test_dir/fixtures/bindings.lua"
