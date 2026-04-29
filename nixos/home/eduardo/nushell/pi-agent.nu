# Experimental pi colon commands for Nushell.
#
# WARNING: this is an early shell-side workflow shim. Session handling,
# command names, and argument behavior may change as the integration evolves.
#
# Examples:
#   : explain this repository
#   : review @flake.nix @home/eduardo/default.nix
#   :new
#   :tui
#   :session

def ":" [...args: string] {
  if ($args | is-empty) {
    error make {
      msg: "Usage: : <prompt>"
      help: "Experimental feature. Use :new for a fresh session or :tui to open pi interactively."
    }
  }

  ^pi-shell prompt ...$args
}

def ":new" [...args: string] {
  ^pi-shell new ...$args
}

def ":tui" [] {
  ^pi-shell tui
}

def ":session" [] {
  ^pi-shell session
}

def ":help" [] {
  ^pi-shell help
}
