# External Integrations

## Core Sections (Required)

### 1) Integration Inventory

| System | Type (API/DB/Queue/etc) | Purpose | Auth model | Criticality | Evidence |
|--------|---------------------------|---------|------------|-------------|----------|
| GitHub-hosted flake inputs (`nixpkgs`, `home-manager`, `disko`, `nixos-anywhere`, etc.) | Dependency source | Supplies NixOS packages, modules, and deployment tooling | Public fetches in flake inputs; GitHub Actions checkout for CI | High for `nixos/` | `nixos/flake.nix`, `.github/workflows/nixos-eval.yml` |
| Homebrew / `brew bundle` | Package manager service | Installs macOS CLI and GUI dependencies from `macos/Brewfile` | Local Homebrew installation and user account | High for `macos/` | `macos/Brewfile`, `macos/install.sh` |
| Mac App Store (`mas`) | Package source | Installs selected macOS apps declared in the Brewfile | Local App Store account via `mas` | Medium for `macos/` | `macos/Brewfile` |
| GPG + git-crypt | Secrets tooling | Unlocks and protects `common/authinfo/.authinfo` | Local GPG private key required | High | `common/authinfo/README.md`, `macos/install.sh`, `.gitleaks.toml` |
| Auth Source in Emacs | Credential lookup | Reads API credentials for LLM providers from authinfo | Local auth-source / `.authinfo` lookup | High for those editor integrations | `common/doom/config.el` |
| Anthropic / OpenAI / DeepSeek / Gemini | External APIs | LLM backends configured in Doom Emacs `gptel` | API keys from `auth-source-pick-first-password` | Medium | `common/doom/config.el` |
| systemd user manager | Local service manager | Enables Omarchy user services such as downloads cleanup and grasp backend | Local user session | Medium | `omarchy/install.sh`, `omarchy/systemd/.config/systemd/user/downloads-clean-at-login.service`, `omarchy/systemd/.config/systemd/user/grasp.service` |
| Grasp backend + browser extension | Local app/browser integration | Captures browser content into `~/org/capture.org` | No auth shown in the service file | Low | `omarchy/systemd/.config/systemd/user/grasp.service`, `omarchy/install.sh` |
| Docker + `nixos/nix` image | Container/runtime | Runs first-install `nixos-anywhere` deploys without requiring local Nix | Local Docker daemon and forwarded SSH agent | Medium | `nixos/deploy.sh` |
| Cachix in GitHub Actions | Binary cache | Speeds NixOS CI builds | GitHub Actions vars/secrets (`CACHIX_CACHE_NAME`, `CACHIX_AUTH_TOKEN`) | Medium | `.github/workflows/nixos-eval.yml` |

### 2) Data Stores

| Store | Role | Access layer | Key risk | Evidence |
|------|------|--------------|----------|----------|
| `common/authinfo/.authinfo` | Encrypted credential store | `git-crypt`, Emacs auth-source, stow / symlink deployment | Repo may be locked or key unavailable on a given machine | `common/authinfo/README.md`, `macos/install.sh`, `nixos/home/eduardo/default.nix`, `common/doom/config.el` |
| `~/org/capture.org` | Local capture target for the grasp backend | `grasp.service` | Hardcoded user-path assumption | `omarchy/systemd/.config/systemd/user/grasp.service` |
| `${XDG_STATE_HOME:-$HOME/.local/state}/pi-shell` | Local session state for experimental pi shell bridge | `nixos/home/eduardo/nushell/pi-shell.sh` | Directory-local session model may change; state is shell-local | `nixos/home/eduardo/nushell/pi-shell.sh` |
| [TODO] No application database/cache configuration was identified | [TODO] | [TODO] | [TODO] | `nixos/flake.nix`, `macos/Brewfile`, `omarchy/install.sh` |

### 3) Secrets and Credentials Handling

- Credential sources: `common/authinfo/.authinfo`, local GPG keyring, Emacs `auth-source`, GitHub Actions secrets/vars for Cachix
- Hardcoding checks: No decrypted secrets were found in the reviewed tracked files, but a specific GPG key ID (`5C9B334784343A49`) is hardcoded in `macos/install.sh`, and several API hostnames are hardcoded in `common/doom/config.el`
- Rotation or lifecycle notes: [TODO] No explicit credential rotation policy or secret lifecycle documentation was found

### 4) Reliability and Failure Behavior

- Retry/backoff behavior: Partial; `grasp.service` has `Restart=on-failure` with `RestartSec=10`, but no general retry policy was identified elsewhere
- Timeout policy: [TODO] No consistent timeout policy was identified in the reviewed files
- Circuit-breaker or fallback behavior: None found; CI has a pull-only Cachix fallback, and Omarchy service enablement skips when the user systemd instance is unavailable

### 5) Observability for Integrations

- Logging around external calls: Limited; installers and helper scripts print plain status/error text, and systemd service logs would come from the user service manager
- Metrics/tracing coverage: No metrics or tracing configuration was identified
- Missing visibility gaps: No centralized observability for installers, no API call tracing for editor-side LLM integrations, and no repo-local integration health dashboard/config was found

### 6) Evidence

- `nixos/flake.nix`
- `.github/workflows/nixos-eval.yml`
- `macos/Brewfile`
- `macos/install.sh`
- `common/authinfo/README.md`
- `.gitleaks.toml`
- `common/doom/config.el`
- `omarchy/install.sh`
- `omarchy/systemd/.config/systemd/user/grasp.service`
- `omarchy/systemd/.config/systemd/user/downloads-clean-at-login.service`
- `nixos/deploy.sh`
- `nixos/home/eduardo/default.nix`
- `nixos/home/eduardo/nushell/pi-shell.sh`
