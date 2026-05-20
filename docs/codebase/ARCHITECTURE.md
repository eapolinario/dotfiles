# Architecture

## Core Sections (Required)

### 1) Architectural Style

- Primary style: Platform-partitioned configuration repository with two execution models: declarative NixOS under `nixos/` and imperative stow-based installers under `omarchy/` and `macos/`
- Why this classification: The repo root is split by platform (`omarchy/`, `macos/`, `nixos/`, `common/`), the NixOS side derives hosts from flake metadata, and the Omarchy/macOS sides apply symlinked configs with shell installers
- Primary constraints:
  - Preserve platform boundaries and only place intentionally shared config under `common/`
  - Preserve stow-managed layouts and honor `XDG_CONFIG_HOME` where applicable
  - Avoid destructive application paths unless explicitly requested (`nixos/deploy.sh`, `make deploy`, `make switch`)

### 2) System Flow

```text
user command -> platform entrypoint -> platform config selection -> filesystem/system application -> optional service enablement / CI validation
```

Evidence-backed flow:

1. A user chooses a platform-specific command from the documented entrypoints in `Makefile`, `readme.org`, or `nixos/Makefile`.
2. For Omarchy/macOS, the shell installer resolves repo-relative package directories and applies them with `stow` into `$HOME` or `$XDG_CONFIG_HOME` (`omarchy/install.sh`, `macos/install.sh`).
3. For NixOS, `nixos/flake.nix` derives `nixosConfigurations` from host metadata, then composes host modules, Home Manager, overlays, and shared modules.
4. User-level NixOS config further links live files from this repo using Home Manager out-of-store symlinks (`nixos/home/eduardo/default.nix`).
5. Platform-specific post-apply actions then run: Omarchy enables user services, macOS runs `brew bundle` and starts the yabai service, and NixOS CI runs flake checks / host evaluation / host builds (`omarchy/install.sh`, `macos/install.sh`, `.github/workflows/nixos-eval.yml`).

### 3) Layer/Module Responsibilities

| Layer or module | Owns | Must not own | Evidence |
|-----------------|------|--------------|----------|
| Root orchestration (`Makefile`, `readme.org`) | Human-facing entrypoints and repo overview | Platform-specific implementation details | `Makefile`, `readme.org` |
| `omarchy/install.sh` | Linux installer flow, stow operations, user service enablement | macOS package management or NixOS host evaluation | `omarchy/install.sh` |
| `macos/install.sh` | macOS stow flow, Homebrew application, service/bootstrap setup | NixOS host composition | `macos/install.sh` |
| `nixos/flake.nix` | Host metadata, flake inputs, outputs, and host/check derivation | macOS or Omarchy imperative install steps | `nixos/flake.nix` |
| `nixos/hosts/*` | Host-specific machine settings such as boot, virtualization, display manager, and host name | Shared defaults that belong in `nixos/modules/common` | `nixos/hosts/fusion-vm/default.nix` |
| `nixos/modules/common` | Shared system defaults for NixOS hosts | Per-host tweaks | `nixos/modules/common/default.nix` |
| `nixos/home/eduardo` | User environment, package set, desktop/user programs, and repo-backed symlinks | System bootloader or disk layout | `nixos/home/eduardo/default.nix` |
| `common/doom` and `common/authinfo` | Shared editor config and encrypted credentials | Platform-only logic | `common/doom/config.el`, `common/authinfo/README.md` |

### 4) Reused Patterns

| Pattern | Where found | Why it exists |
|---------|-------------|---------------|
| Stow-managed mirror trees | `omarchy/*/.config/...`, `macos/*`, `common/doom`, `common/authinfo` | Lets installers project repo files into the correct home/config targets with minimal custom logic |
| Flake-derived host metadata | `nixos/flake.nix` | Keeps host names, systems, and CI defaults in one source of truth |
| Out-of-store symlinking | `nixos/home/eduardo/default.nix` | Makes NixOS user config point directly at repo files such as `common/doom` and `nixos/hypr/hyprland.conf` |
| Guard-and-fail shell helpers | `omarchy/install.sh`, `nixos/home/eduardo/nushell/pi-shell.sh` | Validate prerequisites early and exit with actionable messages |

### 5) Known Architectural Risks

- Mixed imperative (`macos/`, `omarchy/`) and declarative (`nixos/`) application models increase cross-platform drift risk.
- Shared assets plus platform-specific copies require discipline to avoid duplicated behavior and inconsistent fixes.
- Several operational files are large and multi-responsibility (`omarchy/install.sh`, `nixos/home/eduardo/default.nix`, `common/doom/config.el`), which raises change risk.

### 6) Evidence

- `Makefile`
- `readme.org`
- `AGENTS.md`
- `omarchy/install.sh`
- `macos/install.sh`
- `nixos/flake.nix`
- `nixos/Makefile`
- `nixos/hosts/fusion-vm/default.nix`
- `nixos/modules/common/default.nix`
- `nixos/home/eduardo/default.nix`
- `common/doom/config.el`
- `common/authinfo/README.md`
- `.github/workflows/nixos-eval.yml`
