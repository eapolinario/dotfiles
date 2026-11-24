#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_DIR="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-battery-test.XXXXXXXX")"
UNIT_FILE="$REPO_DIR/omarchy/systemd/system/battery-charge-limit.service"
readonly REPO_DIR TEST_DIR UNIT_FILE
trap 'rm -r -- "$TEST_DIR"' EXIT

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

set_model() {
  printf '%s\n' "$1" >"$TEST_DIR/sys_vendor"
  printf '%s\n' "$2" >"$TEST_DIR/product_version"
}

run_conditions() {
  local line
  local -a condition_args=()
  while IFS= read -r line; do
    [[ "$line" == ExecCondition=* ]] || continue
    read -r -a condition_args <<<"${line#ExecCondition=}"
    # Execute only the unit's simple grep guards, with DMI files replaced by fixtures.
    # Never execute its driver-loading or charge-setting commands.
    [[ "${#condition_args[@]}" == 5 && "${condition_args[0]}" == /usr/bin/grep &&
      "${condition_args[1]}" == -Fxq && "${condition_args[2]}" == -- ]] ||
      fail 'unexpected battery service condition command'
    case "${condition_args[4]}" in
      /sys/class/dmi/id/sys_vendor) condition_args[4]="$TEST_DIR/sys_vendor" ;;
      /sys/class/dmi/id/product_version) condition_args[4]="$TEST_DIR/product_version" ;;
      *) fail 'unexpected battery service DMI path' ;;
    esac
    "${condition_args[@]}" || return 1
  done <"$UNIT_FILE"
}

assert_skipped() {
  set_model "$1" "$2"
  if run_conditions >"$TEST_DIR/condition.log" 2>&1; then
    fail "battery service accepted unsupported hardware: $1 / $2"
  fi
}

[[ "$(grep -c '^ExecCondition=' "$UNIT_FILE")" == 2 ]] ||
  fail 'battery service must check both vendor and model'
grep -Fxq 'ConditionPathExists=/usr/bin/omarchy' "$UNIT_FILE" ||
  fail 'battery service must require package-backed Omarchy'
grep -Fxq 'ConditionPathExists=/sys/class/power_supply/BAT0' "$UNIT_FILE" ||
  fail 'battery service must require BAT0'
grep -Fxq 'ExecStartPre=/usr/bin/env modprobe system76_acpi' "$UNIT_FILE" ||
  fail 'battery service must load system76_acpi portably'

set_model System76 gaze16-3050
run_conditions || fail 'battery service rejected the configured Gazelle'
assert_skipped System76 gaze16-3060
assert_skipped System76 gaze17-3050
assert_skipped System76 gaze16-3050-extra
assert_skipped Framework gaze16-3050
assert_skipped System76-other gaze16-3050

set_model System76 gaze16-3050
rm -- "$TEST_DIR/product_version"
if run_conditions >"$TEST_DIR/condition.log" 2>&1; then
  fail 'battery service accepted missing model information'
fi

# Check the requested thresholds and their order without writing to real sysfs.
mapfile -t charge_steps < <(grep '^ExecStart=' "$UNIT_FILE")
[[ "${#charge_steps[@]}" == 2 ]] || fail 'expected two charge-threshold writes'
[[ "${charge_steps[0]}" == *'printf "60\\n" > /sys/class/power_supply/BAT0/charge_control_start_threshold'* ]] ||
  fail 'battery service must set the start threshold to 60 first'
[[ "${charge_steps[1]}" == *'printf "80\\n" > /sys/class/power_supply/BAT0/charge_control_end_threshold'* ]] ||
  fail 'battery service must set the stop threshold to 80 second'

printf 'Battery charge-limit regression checks passed.\n'
