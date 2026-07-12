# GitHub Copilot CLI integration for Nushell.
#
# The account login shell is nushell (see hosts/fusion-vm/default.nix), but the
# Copilot CLI's shell tool drives $SHELL with bash/POSIX semantics and fails to
# start under nu. Override $SHELL to bash just for this invocation.

def copilot [...args: string] {
  with-env { SHELL: "/run/current-system/sw/bin/bash" } { ^copilot ...$args }
}
