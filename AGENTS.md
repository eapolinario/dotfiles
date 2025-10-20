AGENTS guide for this repository (dotfiles; no build step)
- Install: make install-linux (Linux), make install-macos (macOS)
- Linux dry-run: ./install_dotfiles_linux.sh --dry-run; help: ./install_dotfiles_linux.sh --help
- Lint (secrets): make gitleaks; regen baseline (review carefully): make gitleaks-baseline-regen
- Lint (shell): shellcheck **/*.sh; shfmt -d .; format fix: shfmt -i 2 -ci -w .
- Syntax check one script: bash -n path/to/script.sh
- "Single test" analogue: shellcheck path/to/script.sh or shfmt -d path; prefer Linux dry-run above
- macOS health check: brew bundle (or brew bundle check) from repo root

Code style guidelines
- Shell: use bash; shebang must match features; start scripts with: set -euo pipefail
- Naming: lower_snake_case for functions/vars; UPPER_SNAKE for readonly constants; use readonly/local
- Safety: quote expansions; use arrays for globs; prefer [[ ... ]] tests and printf over echo
- Dependencies: validate with require_cmd; exit non‑zero on error; write errors to stderr; add --dry-run when sensible; avoid sudo inside scripts
- Idempotence/XDG: do not overwrite user files blindly; honor $XDG_CONFIG_HOME; keep stow-managed layout intact
- Emacs Lisp (Doom): keep lexical-binding; configure via after!/use-package!; kebab-case names; use map!; add docstrings; keep <100 cols
- Systemd/Hypr/Starship/Yabai: keep changes under their component/.config/... trees; don’t edit generated symlink targets; comment non-obvious options
- Commit hygiene: run make gitleaks before pushing; never commit secrets; review baseline changes before updating
- PRs: keep diffs small and focused; verify Linux dry-run and macOS brew bundle locally
- Tests (future): prefer Bats under test/; run a single file with: bats test/foo.bats