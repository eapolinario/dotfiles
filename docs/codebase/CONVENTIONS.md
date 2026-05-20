# Coding Conventions

## Core Sections (Required)

### 1) Naming Rules

| Item | Rule | Example | Evidence |
|------|------|---------|----------|
| Files | Lowercase platform directories; conventional filenames such as `install.sh`, `default.nix`, `disko.nix`, and dotfiles under stow packages | `omarchy/install.sh`, `nixos/hosts/fusion-vm/default.nix`, `macos/zsh/.zshrc` | `AGENTS.md`, repo file layout |
| Functions/methods | Shell uses `lower_snake_case`; custom Emacs Lisp functions use the `ea/` prefix with kebab-like names | `ensure_linux`, `remove_target_if_identical`, `ea/org-archive-done-tasks` | `AGENTS.md`, `omarchy/install.sh`, `common/doom/+bindings.el` |
| Types/interfaces | [TODO] No user-defined typed language interfaces/classes were identified in the files reviewed | [TODO] | `nixos/flake.nix`, `omarchy/install.sh`, `common/doom/config.el` |
| Constants/env vars | Uppercase snake case for shell constants and environment names | `SCRIPT_DIR`, `DRY_RUN`, `PI_SHELL_STATE_DIR`, `XDG_CONFIG_HOME` | `AGENTS.md`, `omarchy/install.sh`, `nixos/home/eduardo/nushell/pi-shell.sh` |

### 2) Formatting and Linting

- Formatter: `shfmt` for shell (documented in `AGENTS.md`); `nix fmt` for the Nix flake (`nixos/Makefile`)
- Linter: ShellCheck for shell scripts; gitleaks for secrets scanning
- Most relevant enforced rules: `#!/usr/bin/env bash`, `set -euo pipefail`, prefer `[[ ... ]]`, prefer `printf`, quote expansions, use arrays for globs/args, use `after!` / `use-package!` / `map!` in Doom config
- Run commands: `bash -n path/to/script.sh`, `shellcheck path/to/script.sh`, `shfmt -d path/to/script.sh`, `make gitleaks`, `cd nixos && make fmt && make check`

### 3) Import and Module Conventions

- Import grouping/order: Nix uses explicit relative `imports = [ ... ]` lists; Doom Emacs uses `load!`, `after!`, and `use-package!` to compose modules after package load
- Alias vs relative import policy: Relative references are the dominant pattern in Nix and shell; no general aliasing system was identified in the reviewed files
- Public exports/barrel policy: [TODO] Not applicable or not explicitly documented for this repo style

### 4) Error and Logging Conventions

- Error strategy by layer: Shell scripts generally fail fast with `set -euo pipefail`, validate required commands with `require_cmd`, and print actionable error text to stderr before exiting; the Omarchy installer adds dry-run-aware messaging
- Logging style and required context fields: Plain command/status output via `printf` or `echo`; no structured logging format or required context schema was identified
- Sensitive-data redaction rules: No explicit runtime logging redaction policy was found; repo-level protection is handled by `git-crypt` for `common/authinfo/.authinfo` and `gitleaks` scanning

### 5) Testing Conventions

- Test file naming/location rule: [TODO] No dedicated `test/`, `tests/`, `spec/`, or `*.bats` files were found; validation is command-oriented instead
- Mocking strategy norm: [TODO] No test harness or mocking layer was identified
- Coverage expectation: [TODO] No coverage tooling or threshold was identified

### 6) Evidence

- `AGENTS.md`
- `omarchy/install.sh`
- `macos/install.sh`
- `nixos/home/eduardo/nushell/pi-shell.sh`
- `nixos/flake.nix`
- `nixos/Makefile`
- `common/doom/config.el`
- `common/doom/+bindings.el`
- `.github/workflows/shellcheck.yml`
- `.gitleaks.toml`
- `Makefile`
