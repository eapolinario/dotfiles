Agent guide for this repository (`dotfiles`; no build step).

Keep changes small, focused, and repo-specific. Prefer the documented install/check commands over inventing new workflows.

## Repo layout
- `omarchy/` — Arch Linux / omarchy / Hyprland dotfiles and installer
- `macos/` — macOS dotfiles, Brewfile, installer, yabai/skhd/tmux/zsh config
- `nixos/` — flake-based NixOS system + home-manager config and helper Makefile
- `common/` — shared config, especially Doom Emacs and encrypted authinfo
- `readme.org` — repository overview and install notes

## Scope by OS / platform
- Default repository questions to the current OS's directory: `omarchy/` for
  Linux/Hyprland, `nixos/` for NixOS, or `macos/` for macOS. Ignore other OS
  directories unless the user explicitly targets another platform, shared
  configuration, or repository-wide files.
- Keep platform-specific changes inside the corresponding top-level directory whenever possible.
- If the user asks about or mentions `nixos`, assume they want changes under `nixos/` unless they explicitly ask for cross-platform/shared updates.
- If the user asks about or mentions `macos`, assume they want changes under `macos/` unless they explicitly ask for cross-platform/shared updates.
- If the user asks about or mentions `omarchy`, assume they want changes under `omarchy/` unless they explicitly ask for cross-platform/shared updates.
- Use `common/` only for intentionally shared configuration.
- Do not make opportunistic changes in other platform directories just because a similar setting exists there.

## Command output
- Prefer built-in file/search tools over `rtk read`, `rtk grep`, or `rtk find`.
- Batch related commands into one call.
- Treat condensed output as complete; follow the recovery paths given by
  truncated results.
- Re-run a command with `rtk bypass <cmd>` only when its output is unusable:
  unexpectedly empty, inconsistent with its exit code, or garbled.

## Preferred entrypoints
### Repository root
- Install omarchy: `make install-omarchy`
- Omarchy dry-run: `./omarchy/install.sh --dry-run`
- Omarchy help: `./omarchy/install.sh --help`
- Omarchy diagnostics (read-only): `./omarchy/install.sh --doctor`
- Omarchy regression/lint entrypoint: `make check-omarchy`
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
- `omarchy/` behavior or shared editor/agent config: run `make check-omarchy`;
  its installer suites use fixture homes, not the live machine.
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

# RTK (Rust Token Killer) - Token-Optimized Commands

## Golden Rule

**Always prefix commands with `rtk`**. If RTK has a dedicated filter, it uses it. If not, it passes through unchanged. This means RTK is always safe to use.

**Important**: Even in command chains with `&&`, use `rtk`:
```bash
# ❌ Wrong
git add . && git commit -m "msg" && git push

# ✅ Correct
rtk git add . && rtk git commit -m "msg" && rtk git push
```

## RTK Commands by Workflow

### Build & Compile (80-90% savings)
```bash
rtk cargo build         # Cargo build output
rtk cargo check         # Cargo check output
rtk cargo clippy        # Clippy warnings grouped by file (80%)
rtk tsc                 # TypeScript errors grouped by file/code (83%)
rtk lint                # ESLint/Biome violations grouped (84%)
rtk prettier --check    # Files needing format only (70%)
rtk next build          # Next.js build with route metrics (87%)
```

### Test (60-99% savings)
```bash
rtk cargo test          # Cargo test failures only (90%)
rtk go test             # Go test failures only (90%)
rtk jest                # Jest failures only (99.5%)
rtk vitest              # Vitest failures only (99.5%)
rtk playwright test     # Playwright failures only (94%)
rtk pytest              # Python test failures only (90%)
rtk rake test           # Ruby test failures only (90%)
rtk rspec               # RSpec test failures only (60%)
rtk test <cmd>          # Generic test wrapper - failures only
```

### Git (59-80% savings)
```bash
rtk git status          # Compact status
rtk git log             # Compact log (works with all git flags)
rtk git diff            # Compact diff (80%)
rtk git show            # Compact show (80%)
rtk git add             # Ultra-compact confirmations (59%)
rtk git commit          # Ultra-compact confirmations (59%)
rtk git push            # Ultra-compact confirmations
rtk git pull            # Ultra-compact confirmations
rtk git branch          # Compact branch list
rtk git fetch           # Compact fetch
rtk git stash           # Compact stash
rtk git worktree        # Compact worktree
```

Note: Git passthrough works for ALL subcommands, even those not explicitly listed.

### GitHub (26-87% savings)
```bash
rtk gh pr view <num>    # Compact PR view (87%)
rtk gh pr checks        # Compact PR checks (79%)
rtk gh run list         # Compact workflow runs (82%)
rtk gh issue list       # Compact issue list (80%)
rtk gh api              # Compact API responses (26%)
```

### JavaScript/TypeScript Tooling (70-90% savings)
```bash
rtk pnpm list           # Compact dependency tree (70%)
rtk pnpm outdated       # Compact outdated packages (80%)
rtk pnpm install        # Compact install output (90%)
rtk npm run <script>    # Compact npm script output
rtk npx <cmd>           # Compact npx command output
rtk prisma              # Prisma without ASCII art (88%)
```

### Files & Search (60-75% savings)
```bash
rtk ls <path>           # Tree format, compact (65%)
rtk read <file>         # Code reading with filtering (60%)
rtk grep <pattern>      # Search grouped by file (75%)
rtk find <pattern>      # Find grouped by directory (70%)
```

### Analysis & Debug (70-90% savings)
```bash
rtk err <cmd>           # Filter errors only from any command
rtk log <file>          # Deduplicated logs with counts
rtk json <file>         # JSON structure without values
rtk deps                # Dependency overview
rtk env                 # Environment variables compact
rtk summary <cmd>       # Smart summary of command output
rtk diff                # Ultra-compact diffs
```

### Infrastructure (85% savings)
```bash
rtk docker ps           # Compact container list
rtk docker images       # Compact image list
rtk docker logs <c>     # Deduplicated logs
rtk kubectl get         # Compact resource list
rtk kubectl logs        # Deduplicated pod logs
```

### Network (65-70% savings)
```bash
rtk curl <url>          # Compact HTTP responses (70%)
rtk wget <url>          # Compact download output (65%)
```

### Meta Commands
```bash
rtk gain                # View token savings statistics
rtk gain --history      # View command history with savings
rtk discover            # Analyze Claude Code sessions for missed RTK usage
rtk bypass <cmd>        # Run command without filtering (for debugging)
rtk init                # Add RTK instructions to CLAUDE.md
rtk init --global       # Add RTK to ~/.claude/CLAUDE.md
```

## Token Savings Overview

| Category | Commands | Typical Savings |
|----------|----------|-----------------|
| Tests | vitest, playwright, cargo test | 90-99% |
| Build | next, tsc, lint, prettier | 70-87% |
| Git | status, log, diff, add, commit | 59-80% |
| GitHub | gh pr, gh run, gh issue | 26-87% |
| Package Managers | pnpm, npm, npx | 70-90% |
| Files | ls, read, grep, find | 60-75% |
| Infrastructure | docker, kubectl | 85% |
| Network | curl, wget | 65-70% |

Overall average: **60-90% token reduction** on common development operations.
