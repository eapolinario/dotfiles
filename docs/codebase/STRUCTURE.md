# Codebase Structure

## Core Sections (Required)

### 1) Top-Level Map

List only meaningful top-level directories and files.

| Path | Purpose | Evidence |
|------|---------|----------|
| `omarchy/` | Arch/Omarchy config, component installer, operations guide, isolated regression suites | `omarchy/README.md`, `omarchy/install.sh`, `omarchy/tests/` |
| `macos/` | macOS dotfiles, Brewfile, and installer | `readme.org`, `macos/install.sh`, `macos/Brewfile` |
| `nixos/` | Flake-based NixOS system, hosts, modules, and Home Manager config | `readme.org`, `nixos/flake.nix`, `nixos/Makefile` |
| `common/` | Shared configuration, especially Doom Emacs and encrypted authinfo | `readme.org`, `common/authinfo/README.md`, `common/doom/config.el` |
| `.github/workflows/` | CI workflows for gitleaks, ShellCheck, and NixOS validation | `.github/workflows/gitleaks.yml`, `.github/workflows/shellcheck.yml`, `.github/workflows/nixos-eval.yml` |
| `plans/` | Repository planning / improvement tracking | `plans/repo-improvements-2026-04-25.md` |
| `Makefile` | Root convenience entrypoints for installers and gitleaks | `Makefile` |
| `readme.org` | Repository overview and installation notes | `readme.org` |
| `AGENTS.md` | Repo-local coding and validation guidance | `AGENTS.md` |

### 2) Entry Points

- Main runtime entry: No single application entrypoint; operational entrypoints are the root `Makefile`, `omarchy/install.sh`, `macos/install.sh`, and `nixos/flake.nix`/`nixos/Makefile`
- Secondary entry points (worker/cli/jobs): `nixos/deploy.sh`, `nixos/home/eduardo/nushell/pi-shell.sh`, `.github/workflows/*.yml`
- How entry is selected (script/config): The selected platform determines the entry path: root `make install-omarchy` dispatches to `./omarchy/install.sh`, root `make install-macos` dispatches to `./macos/install.sh`, and `cd nixos && make ...` operates on the flake-defined hosts in `nixos/flake.nix`

### 3) Module Boundaries

| Boundary | What belongs here | What must not be here |
|----------|-------------------|------------------------|
| `omarchy/` | Linux/Omarchy/Hyprland-specific configs and the Linux installer | macOS-only or shared config that should live under `common/` |
| `macos/` | macOS-specific package, shell, tmux, yabai, and skhd config | NixOS host/module definitions or Omarchy-specific config |
| `nixos/` | Declarative system config, host metadata, shared NixOS modules, and Home Manager | macOS installer logic or Omarchy stow trees |
| `common/` | Intentionally shared assets such as Doom Emacs and encrypted authinfo | Platform-specific drift fixes that should stay in `omarchy/`, `macos/`, or `nixos/` |
| `.github/workflows/` | CI automation and repository checks | Local machine configuration |

### 4) Naming and Organization Rules

- File naming pattern: mostly lowercase names with conventional platform-specific exceptions like `Makefile`, `Brewfile`, `README.md`, dotfiles such as `.zshrc`, and Nix defaults such as `default.nix` / `disko.nix`
- Directory organization pattern: platform-first at the repo root; within platforms, organization is host-based in `nixos/hosts`, module-based in `nixos/modules`, and component/app-based in `omarchy/` and `macos/`
- Import aliasing or path conventions: Nix files use relative imports such as `./disko.nix` and `./modules/common`; stow-managed packages mirror target filesystem paths like `.config/...` inside each package directory

### 5) Evidence

- `readme.org`
- `AGENTS.md`
- `Makefile`
- `macos/install.sh`
- `macos/Brewfile`
- `nixos/flake.nix`
- `nixos/Makefile`
- `nixos/deploy.sh`
- `nixos/home/eduardo/nushell/pi-shell.sh`
- `common/authinfo/README.md`
- `common/doom/config.el`
- `.github/workflows/gitleaks.yml`
- `.github/workflows/shellcheck.yml`
- `.github/workflows/nixos-eval.yml`
- `plans/repo-improvements-2026-04-25.md`
