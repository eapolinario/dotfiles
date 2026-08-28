Agent guide for this repository (`dotfiles`; no build step).

Keep changes small, focused, and repo-specific. Prefer the documented install/check commands over inventing new workflows.

## Repo layout
- `omarchy/` — Arch Linux / omarchy / Hyprland dotfiles and installer
- `macos/` — macOS dotfiles, Brewfile, installer, yabai/skhd/tmux/zsh config
- `nixos/` — flake-based NixOS system + home-manager config and helper Makefile
- `common/` — shared config, especially Doom Emacs and encrypted authinfo
- `readme.org` — repository overview and install notes

## Scope by OS / platform
- Keep platform-specific changes inside the corresponding top-level directory whenever possible.
- If the user asks about or mentions `nixos`, assume they want changes under `nixos/` unless they explicitly ask for cross-platform/shared updates.
- If the user asks about or mentions `macos`, assume they want changes under `macos/` unless they explicitly ask for cross-platform/shared updates.
- If the user asks about or mentions `omarchy`, assume they want changes under `omarchy/` unless they explicitly ask for cross-platform/shared updates.
- Use `common/` only for intentionally shared configuration.
- Do not make opportunistic changes in other platform directories just because a similar setting exists there.

## Preferred entrypoints
### Repository root
- Install omarchy: `make install-omarchy`
- Omarchy dry-run: `./omarchy/install.sh --dry-run`
- Omarchy help: `./omarchy/install.sh --help`
- Install macOS config: `make install-macos`
- Secret scan: `make gitleaks`
- Regenerate gitleaks baseline only after review: `make gitleaks-baseline-regen`
- Update Brewfile from a live macOS machine: `make brewfile-update`

### Shell validation
- Syntax check one script: `bash -n path/to/script.sh`
- Lint one script: `shellcheck path/to/script.sh`
- Check formatting one path: `shfmt -d path/to/script.sh`
- Format shell files: `shfmt -i 2 -ci -w .`
- Lint shell files: `shellcheck **/*.sh`

### macOS
- Health check from `macos/`: `brew bundle check`
- Apply Brewfile from `macos/`: `brew bundle`

### NixOS (`cd nixos`)
- Format: `make fmt`
- Flake checks: `make flake-check`
- Evaluate configured hosts: `make eval-hosts`
- Main validation: `make check`
- Switch local host config: `make switch HOST=<name>`
- Build one host: `make build-host HOST=<name>`

## Validation by change area
- Shell scripts (`*.sh`): run `bash -n`, `shellcheck`, and `shfmt -d` on touched files.
- `omarchy/` installer or stow-managed config: run `./omarchy/install.sh --dry-run`.
- `macos/install.sh` or `macos/Brewfile`: run `brew bundle check` from `macos/` when on macOS.
- `nixos/*.nix`, `nixos/modules`, `nixos/hosts`, `nixos/home`: run `cd nixos && make fmt && make check`.
- Secrets-related files (`common/authinfo`, `.gitleaks.toml`, `gitleaks.baseline`): run `make gitleaks`.

## Safety and repo-specific constraints
- Preserve the stow-managed layout; do not edit generated symlink targets in `$HOME` or under live config directories.
- Honor `XDG_CONFIG_HOME` where applicable.
- Do not overwrite user files blindly; keep installer behavior idempotent.
- Avoid `sudo` inside scripts; validate dependencies with `require_cmd` and fail with a non-zero exit code.
- Add `--dry-run` support when introducing filesystem-changing script behavior.
- Keep component-specific changes under their own trees, e.g. `omarchy/hypr/.config/hypr`, `omarchy/systemd/.config/systemd`, `macos/yabai`, `macos/skhd`, `common/doom`, `common/copilot/.copilot`.
- `common/copilot/.copilot/copilot-instructions.md` is the *global* agent instruction file, loaded in every repository on this machine. Keep it short and machine-wide; anything repo-specific belongs in that repo's own `AGENTS.md`.
- Comment non-obvious Hyprland, systemd, starship, skhd, and yabai settings.

## Code style
### Shell
- Use bash.
- Shebang: `#!/usr/bin/env bash`
- Start scripts with: `set -euo pipefail`
- Prefer `[[ ... ]]` over `[` where possible.
- Prefer `printf` over `echo`.
- Quote expansions.
- Use arrays for globs and argument lists.
- Use `lower_snake_case` for functions and variables.
- Use `UPPER_SNAKE_CASE` for readonly constants.
- Use `readonly` and `local` deliberately.

### Emacs Lisp (Doom)
- Keep `lexical-binding`.
- Configure via `after!` / `use-package!`.
- Use kebab-case names.
- Prefer `map!` for keybindings.
- Add docstrings.
- Keep lines under 100 columns.

## Secrets and encrypted files
- `common/authinfo/.authinfo` is encrypted with `git-crypt`.
- If the repo is locked, use `git-crypt unlock` before assuming authinfo contents are available.
- Never commit decrypted secrets or copy secret material into new tracked files.
- Review changes to `gitleaks.baseline` carefully; baseline updates should only reflect intentional, understood changes.
- macOS and NixOS authinfo behavior may differ when the repo is still locked; do not “fix” this by committing decrypted data.

## Destructive operations
- `nixos/deploy.sh` and `cd nixos && make deploy` are first-install workflows built around `nixos-anywhere`/`disko`; they can wipe the target disk. Do not run them unless the user explicitly asks and the target is confirmed.
- `cd nixos && make switch` mutates the current machine; prefer `make check` unless the user asked to apply changes.
- Installers change the local environment; prefer dry-runs and validation when the task is only to edit files.

## Change hygiene
- Keep diffs minimal and focused.
- Avoid unrelated reformatting.
- Mention the validation commands you ran.
- Run `make gitleaks` before pushing changes that touch secrets-related areas.
