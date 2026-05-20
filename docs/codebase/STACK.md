# Technology Stack

## Core Sections (Required)

### 1) Runtime Summary

| Area | Value | Evidence |
|------|-------|----------|
| Primary language | Mixed repository: Nix for NixOS configuration, Bash for installers/helpers, and Emacs Lisp for shared Doom Emacs config | `nixos/flake.nix`, `omarchy/install.sh`, `macos/install.sh`, `common/doom/config.el` |
| Runtime + version | Multi-runtime repo: NixOS flake input tracks `nixos-unstable`; NixOS configs set `system.stateVersion = "24.11"`; shell entrypoints use `/usr/bin/env bash` | `nixos/flake.nix`, `nixos/hosts/fusion-vm/default.nix`, `omarchy/install.sh`, `macos/install.sh`, `nixos/deploy.sh` |
| Package manager | Nix flakes for NixOS, Homebrew Bundle for macOS, and GNU Stow for file deployment on macOS/Omarchy | `nixos/flake.nix`, `macos/Brewfile`, `macos/install.sh`, `omarchy/install.sh` |
| Module/build system | Nix flake outputs plus Makefile targets; stow-managed directory trees mirror target paths under `$HOME` / `$XDG_CONFIG_HOME` | `nixos/flake.nix`, `nixos/Makefile`, `Makefile`, `omarchy/install.sh`, `macos/install.sh` |

### 2) Production Frameworks and Dependencies

List only high-impact production dependencies (frameworks, data, transport, auth).

| Dependency | Version | Role in system | Evidence |
|------------|---------|----------------|----------|
| Nixpkgs / NixOS | `nixos-unstable` | Base package set and module library for `nixos/` | `nixos/flake.nix` |
| Home Manager | [TODO exact version; flake input is pinned in `flake.lock`] | User-level NixOS/home environment management | `nixos/flake.nix`, `nixos/home/eduardo/default.nix` |
| Disko | [TODO exact version; flake input is pinned in `flake.lock`] | Disk layout provisioning for first-install workflows | `nixos/flake.nix`, `nixos/hosts/fusion-vm/disko.nix`, `nixos/deploy.sh` |
| nixos-anywhere | [TODO exact version; flake input is pinned in `flake.lock`] | Remote first-install deployment for NixOS hosts | `nixos/flake.nix`, `nixos/deploy.sh` |
| GNU Stow | [TODO version] | Symlink-based deployment for `omarchy/`, `macos/`, and shared `common/` trees | `omarchy/install.sh`, `macos/install.sh` |
| Doom Emacs | [TODO version] | Shared editor configuration deployed from `common/doom` | `readme.org`, `common/doom/config.el`, `macos/install.sh` |
| git-crypt | [TODO version] | Encryption for `common/authinfo/.authinfo` | `common/authinfo/README.md`, `.gitleaks.toml`, `macos/install.sh` |

### 3) Development Toolchain

| Tool | Purpose | Evidence |
|------|---------|----------|
| ShellCheck | Shell linting in CI and local validation guidance | `.github/workflows/shellcheck.yml`, `AGENTS.md` |
| `shfmt` | Shell formatting check/format guidance | `AGENTS.md` |
| `gitleaks` | Secret scanning with a baseline | `Makefile`, `.gitleaks.toml`, `.github/workflows/gitleaks.yml` |
| `nix fmt` | Nix formatting | `nixos/Makefile` |
| `nix flake check` / host eval / host build | NixOS flake validation and build verification | `nixos/Makefile`, `.github/workflows/nixos-eval.yml` |
| `brew bundle check` | macOS package validation | `AGENTS.md` |

### 4) Key Commands

```bash
make install-omarchy
./omarchy/install.sh --dry-run
make install-macos
make gitleaks
cd nixos && make check
```

### 5) Environment and Config

- Config sources: `readme.org`, `AGENTS.md`, `omarchy/install.sh`, `macos/install.sh`, `nixos/flake.nix`, `nixos/Makefile`, `nixos/home/eduardo/default.nix`, `common/doom/config.el`, `common/authinfo/README.md`
- Required env vars: `HOME`, `XDG_CONFIG_HOME` (fallback-aware in installers), `XDG_STATE_HOME` (for `pi-shell` state), `TARGET` for `cd nixos && make deploy`, `[TODO any additional host-local env not committed]`
- Deployment/runtime constraints: Omarchy installer requires Linux, `stow`, and `systemctl`; macOS installer requires `gpg`, the configured private key, and `brew`; NixOS workflows require Nix flakes and host metadata from `nixos/flake.nix`

### 6) Evidence

- `Makefile`
- `readme.org`
- `AGENTS.md`
- `macos/Brewfile`
- `macos/install.sh`
- `omarchy/install.sh`
- `nixos/flake.nix`
- `nixos/Makefile`
- `nixos/home/eduardo/default.nix`
- `common/authinfo/README.md`
- `.gitleaks.toml`
- `.github/workflows/shellcheck.yml`
- `.github/workflows/gitleaks.yml`
- `.github/workflows/nixos-eval.yml`
