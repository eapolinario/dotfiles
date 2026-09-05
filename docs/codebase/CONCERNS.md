# Codebase Concerns

Omarchy/shared-config entries have been refreshed for the component installer
and regression suites. Other platform notes below are historical review
observations, not a new assessment of macOS or NixOS.

## Core Sections (Required)

### 1) Top Risks (Prioritized)

| Severity | Concern | Evidence | Impact | Suggested action |
|----------|---------|----------|--------|------------------|
| High | `readme.org` says locked/unkeyed installs will silently skip authinfo on macOS, but `macos/install.sh` exits if `gpg` or the configured key is missing | `readme.org`, `macos/install.sh`, `plans/repo-improvements-2026-04-25.md` | macOS installs can fail unexpectedly when the repo is locked or the key is unavailable | Align docs and installer behavior; choose one intended policy |
| High | `nixos/deploy.sh` and `cd nixos && make deploy` are first-install workflows that can wipe a target disk | `AGENTS.md`, `nixos/deploy.sh`, `nixos/Makefile` | Accidental destructive deployment if invoked casually or with the wrong target | Keep strong operator confirmation and avoid using deploy paths for routine validation |
| High | Opted-in Downloads cleanup permanently deletes contents, and service activation is outside file rollback | `omarchy/install.sh`, `omarchy/README.md` | Deleted downloads cannot be restored from configuration backups | Keep cleanup explicitly selected, review directory/retention, and inspect the activation plan |
| Medium | Omarchy CI covers fixtures, not a live desktop or complete editor/plugin bootstrap | `.github/workflows/omarchy-ci.yml`, `omarchy/tests/`, `docs/codebase/TESTING.md` | Upstream compositor/plugin behavior can diverge from isolated expectations | Review upstream changes and perform deliberate live validation before deploying |
| Medium | Several important files are large and multi-responsibility (`omarchy/install.sh`, `nixos/home/eduardo/default.nix`, `common/doom/config.el`) | `omarchy/install.sh`, `nixos/home/eduardo/default.nix`, `common/doom/config.el` | Small changes are harder to isolate and review safely | Split by responsibility when the next targeted refactor is needed |

### 2) Technical Debt

List the most important debt items only.

| Debt item | Why it exists | Where | Risk if ignored | Suggested fix |
|-----------|---------------|-------|-----------------|---------------|
| Older imperative macOS installer | Planning notes explicitly call out `macos/install.sh` as older in style and less structured than `omarchy/install.sh` | `macos/install.sh`, `plans/repo-improvements-2026-04-25.md` | More install-time surprises, harder idempotence/dry-run work, and harder maintenance | Refactor toward functions, `require_cmd`, and dry-run support |
| Shared/experimental config churn in NixOS + Hyprland paths | Planning notes and recent churn both point to experimentation in active NixOS/Hyprland files | `plans/repo-improvements-2026-04-25.md`, `nixos/hypr/hyprland.conf`, `nixos/home/eduardo/default.nix`, `nixos/hosts/fusion-vm/default.nix` | Shared paths become fragile and host-specific work leaks into common config | Introduce clearer host-specific overrides or experimental fragments |
| Installer transaction complexity | Backup, rollback, legacy folding and local-state preservation interact | `omarchy/install.sh`, `omarchy/tests/install.sh` | A new component can bypass a safety invariant if wired independently | Reuse the existing plan/preflight/apply path and extend isolated fixtures |

### 3) Security Concerns

| Risk | OWASP category (if applicable) | Evidence | Current mitigation | Gap |
|------|--------------------------------|----------|--------------------|-----|
| Passwordless sudo for wheel users on NixOS hosts | N/A | `nixos/modules/common/default.nix` | SSH password auth is disabled and root SSH login is denied | Local privilege escalation still becomes easier for any compromised wheel account |
| `nixos-anywhere` deploy disables strict host key checking | N/A | `nixos/deploy.sh` | Intended first-install convenience with SSH agent forwarding | Host authenticity is not verified during deploy |
| macOS installer depends on a hardcoded GPG key ID | N/A | `macos/install.sh` | Explicit preflight check prevents proceeding without that key | Secret access policy is coupled to one configured key identifier |

### 4) Performance and Scaling Concerns

| Concern | Evidence | Current symptom | Scaling risk | Suggested improvement |
|---------|----------|-----------------|-------------|-----------------------|
| Sequential bootstrap work in `macos/install.sh` | `macos/install.sh` | Fresh or repeated bootstrap does many network/package steps serially (`git clone`, `brew bundle`, plugin/theme installs) | Slow setup and more partial-failure surface as bootstrap logic grows | Split expensive steps, add dry-run/reporting, and make reruns more granular |
| NixOS build validation cost is non-trivial enough to require cache support | `.github/workflows/nixos-eval.yml`, `plans/repo-improvements-2026-04-25.md` | CI explicitly configures Cachix and separate ARM64 builds | Build time and CI cost will grow with more hosts/packages | Keep cache metadata centralized and limit build scope intentionally |
| Large config files concentrate unrelated edits | `nixos/home/eduardo/default.nix`, `common/doom/config.el`, `omarchy/install.sh` | Active files accumulate many unrelated settings | Merge conflicts and review cost increase with repo growth | Continue splitting large files by feature or platform concern |

### 5) Fragile/High-Churn Areas

| Area | Why fragile | Churn signal | Safe change strategy |
|------|-------------|-------------|----------------------|
| `nixos/home/eduardo/default.nix` | Large user-environment aggregation file with packages, symlinks, desktop config, and shell config | `git log --since='90 days ago'` shows 44 path occurrences | Make narrow edits, validate with `cd nixos && make check`, and prefer extracting cohesive submodules |
| `nixos/hosts/fusion-vm/default.nix` | Host-specific virtualization and graphics workarounds can be environment-sensitive | `git log --since='90 days ago'` shows 39 path occurrences | Keep VM-specific assumptions documented and isolate host-only changes here |
| `nixos/modules/common/default.nix` | Shared defaults affect every NixOS host | `git log --since='90 days ago'` shows 20 path occurrences | Change conservatively and validate all declared hosts |
| `nixos/hypr/hyprland.conf` | Interactive desktop behavior is sensitive to hardware/session differences | `git log --since='90 days ago'` shows 19 path occurrences | Comment non-obvious settings and separate experiments where possible |
| `nixos/flake.nix` | Central source of host metadata and flake wiring | `git log --since='90 days ago'` shows 17 path occurrences | Keep metadata changes small and verify both local and CI consumers |

### 6) `[ASK USER]` Questions

1. [ASK USER] Should macOS authinfo handling fail fast when the GPG key is missing, or should it match `readme.org` and skip authinfo gracefully with a warning?
2. [ASK USER] Is `security.sudo.wheelNeedsPassword = false` an intentional long-term default for all NixOS hosts, or only acceptable for selected development/VM contexts?

The earlier Omarchy CI question is resolved: `make check-omarchy` runs through
`.github/workflows/omarchy-ci.yml`, including full-install dry-run fixtures.

### 7) Evidence

- `readme.org`
- `AGENTS.md`
- `macos/install.sh`
- `omarchy/install.sh`
- `nixos/deploy.sh`
- `nixos/Makefile`
- `nixos/flake.nix`
- `nixos/home/eduardo/default.nix`
- `nixos/hosts/fusion-vm/default.nix`
- `nixos/modules/common/default.nix`
- `nixos/hypr/hyprland.conf`
- `common/doom/config.el`
- `.github/workflows/shellcheck.yml`
- `.github/workflows/omarchy-ci.yml`
- `omarchy/tests/install.sh`
- `omarchy/README.md`
- `.github/workflows/gitleaks.yml`
- `.github/workflows/nixos-eval.yml`
- `plans/repo-improvements-2026-04-25.md`
