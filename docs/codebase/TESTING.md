# Testing patterns

## Linux and shared-configuration entrypoints

From the repository root:

```sh
make check-omarchy   # syntax, ShellCheck, shfmt, unit definitions, regression suites
make test-omarchy    # standalone Bash, headless Neovim Lua, and Emacs ERT suites
make gitleaks        # history scan against the reviewed baseline
```

`SHFMT`, `NVIM`, and `EMACS` can be overridden to use existing binaries in
nonstandard locations. No additional test framework is required. The Bash
suites use explicit assertions, Lua suites use Neovim's built-in APIs, and
Emacs supplies ERT.

Other platform entrypoints remain documented in `AGENTS.md`; these Linux suites
do not replace a macOS bootstrap or NixOS host evaluation.

## Layout and coverage

| Area | Scope |
|------|-------|
| `omarchy/tests/install.sh` | Full-install dry-run snapshots, custom XDG paths, components, shared-skill discovery, local state, locked/unlocked fixture credentials, service opt-ins, private cleanup rules, canonical path guards, legacy directory folding, file rollback, diagnostics |
| `omarchy/tests/nvim-install.sh` | Seeded Omarchy support, legacy file links, generated theme/lock exclusions, directory conflicts, backups, repeated installation |
| `omarchy/tests/nvim-config.lua` | Omarchy, NixOS, macOS and generic Linux branches; picker ownership, directory searches, extras and blame settings |
| Workspace suites in `omarchy/tests/` | Window-set swapping, invalid/cancelled input, dispatch errors and XDG-aware binding paths without a real desktop session |
| Doom ERT suite in `omarchy/tests/` | Per-buffer download paths and agent-shell configuration lifecycle without bootstrapping Doom or contacting providers |

Standalone suites live directly under `omarchy/tests/` and are discovered by
the Makefile by extension. Supporting fixtures belong in subdirectories.
`.github/workflows/omarchy-ci.yml` runs the same umbrella target for Linux/shared
changes. `.github/workflows/shellcheck.yml` retains repository-wide shell linting.

## Isolation

Bash installer tests copy configuration into temporary repositories and redirect
`HOME` and `XDG_CONFIG_HOME`. They create synthetic authinfo fixtures rather than
copying real credentials. Service and desktop commands are mocked; Stow itself
is real. Cleanup rule parsing uses a nonmatching `--prefix`, never a live cleanup
directory. Temporary fixture trees are removed on exit.

Neovim runs with `--headless -u NONE -i NONE`: normal configuration and plugin
bootstrap are not loaded. ERT runs in batch mode without personal init files.
Do not replace these fixtures with a live installer invocation in CI.

## Limitations

There is no numeric coverage threshold. These suites cover configuration
contracts and command interactions, not a running Hyprland compositor, a
complete Doom/LazyVim plugin installation, provider authentication, or real
service startup/network provisioning. Service activation and destructive
cleanup remain explicit operator actions.

## Evidence

- `Makefile`
- `omarchy/tests/`
- `omarchy/install.sh`
- `.github/workflows/omarchy-ci.yml`
- `.github/workflows/shellcheck.yml`
- `.github/workflows/gitleaks.yml`
- `omarchy/README.md`
