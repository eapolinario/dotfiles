#!/usr/bin/env bash
set -euo pipefail

PATH="/usr/bin:/bin:/usr/sbin:/sbin${PATH:+:$PATH}"

require_cmd() {
  local cmd="$1"

  if ! command -v "$cmd" >/dev/null 2>&1; then
    printf 'Missing required command: %s\n' "$cmd" >&2
    exit 1
  fi
}

require_cmd awk
require_cmd sysctl
require_cmd top
require_cmd vm_stat

cpu_usage="$(
  top -l 1 -n 0 2>/dev/null | awk -F'[:, ]+' '
    /CPU usage/ {
      for (i = 1; i <= NF; i++) {
        if ($i ~ /idle/) {
          idle = $(i - 1)
          sub(/%/, "", idle)
          printf "%.0f", 100 - idle
          exit
        }
      }
    }
  ' || true
)"

if [[ -z "$cpu_usage" ]]; then
  printf 'Failed to read CPU usage\n' >&2
  exit 1
fi

vm_stat_output="$(vm_stat 2>/dev/null || true)"
if [[ -z "$vm_stat_output" ]]; then
  printf 'Failed to read vm_stat output\n' >&2
  exit 1
fi

active_pages="$(awk '/Pages active/ {gsub("\\.", "", $(NF)); print $(NF); exit}' <<<"$vm_stat_output")"
wired_pages="$(awk '/Pages wired/ {gsub("\\.", "", $(NF)); print $(NF); exit}' <<<"$vm_stat_output")"
compressed_pages="$(awk '/Pages occupied by compressor/ {gsub("\\.", "", $(NF)); print $(NF); exit}' <<<"$vm_stat_output")"

active_pages="${active_pages:-0}"
wired_pages="${wired_pages:-0}"
compressed_pages="${compressed_pages:-0}"

page_size="$(sysctl -n hw.pagesize 2>/dev/null || true)"
mem_size="$(sysctl -n hw.memsize 2>/dev/null || true)"
if [[ -z "$page_size" || -z "$mem_size" ]]; then
  printf 'Failed to read memory size info\n' >&2
  exit 1
fi

mem_usage="$(
  awk -v active="$active_pages" \
      -v wired="$wired_pages" \
      -v compressed="$compressed_pages" \
      -v page_size="$page_size" \
      -v mem_size="$mem_size" '
    BEGIN {
      used = (active + wired + compressed) * page_size
      if (mem_size == 0) {
        printf "0"
        exit
      }
      printf "%.0f", (used / mem_size) * 100
    }
  '
)"

cpu_usage="${cpu_usage:-0}"
mem_usage="${mem_usage:-0}"

printf 'CPU %s%% MEM %s%%' "$cpu_usage" "$mem_usage"
