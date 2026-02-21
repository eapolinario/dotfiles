#!/usr/bin/env bash
# Deploy a NixOS configuration to a target machine using nixos-anywhere.
# Runs inside a nixos/nix Docker container — no local nix install required.
#
# Usage: ./deploy.sh <hostname> <user@ip>
# Example: ./deploy.sh mymachine root@192.168.1.100
#
# Prerequisites:
#   - Docker running locally
#   - SSH access to the target (booted into a Linux live system or existing NixOS)

set -euo pipefail

HOST="${1:?Usage: $0 <hostname> <user@ip>}"
TARGET="${2:?Usage: $0 <hostname> <user@ip>}"

FLAKE_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Deploying nixos#${HOST} to ${TARGET}..."

docker run --rm \
  -v "${FLAKE_DIR}:/nixos" \
  --mount "type=bind,src=/run/host-services/ssh-auth.sock,target=/ssh-agent" \
  -e SSH_AUTH_SOCK=/ssh-agent \
  nixos/nix \
  sh -c "
    mkdir -p /etc/nix
    echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
    nix run github:nix-community/nixos-anywhere -- \
      --ssh-option StrictHostKeyChecking=no \
      --phases disko,install,reboot \
      --flake /nixos#${HOST} \
      ${TARGET}
  "
