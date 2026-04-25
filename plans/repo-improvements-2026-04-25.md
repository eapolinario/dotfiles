# Repo improvement tracker

Date: 2026-04-25

## How to use this file

Status values:
- `todo`
- `in-progress`
- `blocked`
- `done`
- `wont-do`

When work starts or finishes, update the task entry directly.

## Current focus

- Primary environment now: `nixos/`
- Tracking format: this Markdown file is the source of truth
- Current active task: Add NixOS evaluation checks

## Task tracker

### Quick wins

- [ ] Add `.mailmap`
  - Status: `todo`
  - Priority: high
  - Scope: repo-wide
  - Why: multiple author identities appear in git history
  - Notes:

- [ ] Reconcile authinfo docs vs installer behavior
  - Status: `todo`
  - Priority: high
  - Scope: `readme.org`, `macos/install.sh`, possibly `omarchy/install.sh`
  - Why: docs imply graceful skip; macOS installer currently hard-fails if GPG/key is unavailable
  - Preferred direction: graceful skip with a clear warning
  - Notes:

- [ ] Add compatibility notes to docs
  - Status: `todo`
  - Priority: medium
  - Scope: docs
  - Why: macOS/yabai/skhd and Hyprland-related integrations are sensitive to upstream changes
  - Suggested notes:
    - tested macOS version
    - tested yabai/skhd assumptions
    - tested Omarchy/Hyprland assumptions
    - tested NixOS channel / flake baseline
  - Notes:

- [ ] Add an Omarchy dry-run check to CI
  - Status: `todo`
  - Priority: high
  - Scope: `.github/workflows/`
  - Why: CI currently validates shell/secrets, but not installer behavior
  - Validation target: `./omarchy/install.sh --dry-run`
  - Notes:

### Medium effort

- [ ] Refactor `macos/install.sh`
  - Status: `todo`
  - Priority: high
  - Scope: `macos/install.sh`
  - Why: it is older in style and less structured than `omarchy/install.sh`
  - Goals:
    - add `--dry-run`
    - split logic into functions
    - add `require_cmd`
    - improve idempotence and conflict handling
    - make side effects easier to understand and optionally skip
  - Notes:

- [ ] Add `brew bundle check` validation
  - Status: `todo`
  - Priority: medium
  - Scope: CI and/or local make target
  - Why: catches macOS package drift earlier
  - Notes:

- [ ] Add NixOS evaluation checks
  - Status: `in-progress`
  - Priority: high
  - Scope: `nixos/`, CI
  - Why: `nixos/` is now the most active part of the repo
  - Suggested checks:
    - `nix flake check`
    - host evaluation for declared systems
  - Notes:
    - Added local `make flake-check`, `make eval-hosts`, and `make check` targets in `nixos/Makefile`
    - Added GitHub Actions workflow `.github/workflows/nixos-eval.yml` to run `make check`
    - Kept `HOSTS` explicit for now and documented that it should stay aligned with flake hosts until CI host discovery is automated

- [ ] Introduce a clearer mechanism for experiments and host-specific tweaks
  - Status: `todo`
  - Priority: medium
  - Scope: Hyprland/NixOS/common config layout
  - Why: git history shows experiment/revert churn in shared config
  - Options:
    - host-specific overrides
    - feature flags
    - `experimental/` fragments
    - keep VM-only tweaks out of shared paths
  - Notes:

### Larger structural improvements

- [ ] Add Bats smoke tests for installers
  - Status: `todo`
  - Priority: medium
  - Scope: future `test/` directory, shell scripts
  - Why: installers are important operational logic and change over time
  - Suggested test cases:
    - argument parsing
    - missing dependency failures
    - dry-run behavior
    - idempotent reruns
    - authinfo present vs absent
    - stow conflict handling
  - Notes:

- [ ] Add richer Nix flake checks
  - Status: `todo`
  - Priority: medium
  - Scope: `nixos/flake.nix`
  - Why: NixOS config now deserves project-grade validation
  - Notes:

- [ ] Document intended cross-platform feature parity
  - Status: `todo`
  - Priority: low
  - Scope: docs
  - Why: mixed imperative/declarative approaches increase drift risk
  - Examples:
    - shell/editor/search workflows
    - authinfo/secrets handling
    - prompt/tooling expectations
  - Notes:

## Background and rationale

### Repo summary

This repository is a cross-platform dotfiles setup covering:

- `omarchy/` for Arch Linux / Omarchy / Hyprland
- `macos/` for macOS
- `nixos/` for NixOS + Home Manager
- `common/` for shared configuration, especially Doom Emacs and encrypted authinfo

Recent history suggests the repo has evolved from a simple personal dotfiles collection into a multi-platform workstation/bootstrap repository, with most current activity concentrated in `nixos/`.

### Observations from git history

1. **NixOS has become the most active area**
   - Recent commits are heavily concentrated in `nixos/`.
   - This part of the repo now behaves more like an actively maintained configuration project than just dotfiles.

2. **A recent structural refactor improved clarity**
   - The move to per-platform top-level directories was a strong improvement.

3. **Hyprland-related changes show experimentation on `main`**
   - The history includes several experiment/revert cycles around cursor warping and VMware behavior.
   - This suggests a need for better isolation of hardware-specific or experimental changes.

4. **macOS support is maintained but comparatively imperative and reactive**
   - Recent changes were mostly compatibility fixes around yabai/skhd/service startup.
   - `macos/install.sh` is older in style than `omarchy/install.sh`.

5. **Repo quality practices are improving**
   - ShellCheck CI and gitleaks are in place.
   - Shell scripts have already started moving toward safer patterns.

## Suggested next implementation step

Start by bringing `macos/install.sh` closer to the structure and safety level of `omarchy/install.sh`, while also clarifying authinfo behavior. That gives immediate maintainability benefits and reduces install-time surprises.
