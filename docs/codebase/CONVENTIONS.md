# Coding Conventions

## Core Sections (Required)

### 1) Naming Rules

| Item | Rule | Example | Evidence |
|------|------|---------|----------|
| Files | Lowercase platform directories; conventional filenames such as `install.sh`, `default.nix`, `disko.nix`, and dotfiles under stow packages | `omarchy/install.sh`, `nixos/hosts/fusion-vm/default.nix`, `macos/zsh/.zshrc` | `AGENTS.md`, repo file layout |
| Functions/methods | Shell uses `lower_snake_case`; custom Emacs Lisp uses prefixed kebab-case names | `preflight`, `ea/org-archive-done-tasks`, `my-org-download-set-image-dir` | `AGENTS.md`, `omarchy/install.sh`, `common/doom/+bindings.el`, `common/doom/+org.el` |
| Types/interfaces | pi extensions use TypeScript interfaces and literal unions | `CavemanState`, `Level` | `common/pi/.pi/agent/extensions/caveman-status.ts` |
| Constants/env vars | Uppercase snake case for readonly constants/environment names; lowercase for mutable shell state | `SCRIPT_DIR`, `DEFAULT_COMPONENTS`, `dry_run`, `XDG_CONFIG_HOME` | `AGENTS.md`, `omarchy/install.sh` |

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
- Sensitive-data handling: the Omarchy installer reads only the git-crypt header when checking locked authinfo, never logs credential contents, and never unlocks the repo. Protection at rest/history is handled by git-crypt and gitleaks scanning.

### 5) Testing Conventions

- Test layout: standalone Bash, Lua, and Emacs Lisp suites live directly under `omarchy/tests/`; the Makefile discovers them by extension.
- Isolation: fixture homes and synthetic credentials; mock service/desktop commands; real Stow; headless Neovim without user configuration; built-in Emacs ERT without a Doom bootstrap.
- Coverage expectation: add a regression for changed behavior; no numeric coverage threshold is configured. `make check-omarchy` is shared by local development and Linux CI.

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
