#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DROP_IN="$REPO_DIR/omarchy/systemd/system/tailscaled.service.d/10-omarchy.conf"
readonly REPO_DIR DROP_IN

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

grep -Fxq 'Wants=network-online.target' "$DROP_IN" ||
  fail 'Tailscale drop-in must wait for network-online'
grep -Fxq 'After=network-online.target' "$DROP_IN" ||
  fail 'Tailscale drop-in must start after network-online'
grep -Fxq 'Restart=on-failure' "$DROP_IN" ||
  fail 'Tailscale drop-in must restart failed daemon'
grep -Fxq 'RestartSec=5s' "$DROP_IN" ||
  fail 'Tailscale drop-in must use a five-second restart delay'

if grep -Eq '^Exec(Start|StartPre|StartPost)=|^ExecStop=' "$DROP_IN"; then
  fail 'Tailscale drop-in must preserve package-owned daemon commands'
fi

printf 'Tailscale daemon drop-in regression checks passed.\n'
