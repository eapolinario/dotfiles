# Dotfiles

Personal configuration for three platforms (NixOS, macOS, omarchy/Arch) sharing
a single repository. The hard problem is keeping cross-platform pieces (agent
CLI configs, shared skills, Doom Emacs, encrypted authinfo) consistent across
two materialization paths: GNU Stow on macOS/omarchy and home-manager on NixOS.

## Language

**Skill**:
A self-contained agent prompt package at `common/skills/<name>/`, marked by a
`SKILL.md` at its root.
_Avoid_: plugin, prompt, capability.

**Shared skill**:
Same as **Skill** in this repo. Omarchy exposes every shared skill to each
selected agent CLI at installation time. Other platforms have their own
materialization paths; placing a skill here alone does not install it everywhere.
_Avoid_: common skill.

**Agent CLI**:
A coding-agent terminal application that consumes skills and per-agent config
under its own home subdirectory: Claude Code (`~/.claude`), GitHub Copilot CLI
(`~/.copilot`), pi (`~/.pi`).
_Avoid_: assistant, bot, AI tool.

**Stow tree**:
A subdirectory of `common/<package>/` or a platform directory whose layout
mirrors a home/config destination. Omarchy targets each component directory
explicitly with `--no-folding`, honoring `XDG_CONFIG_HOME` for XDG components.
_Avoid_: package, dotfile dir.

**HM-managed home file**:
A file under `~/` materialized by home-manager via `home.file.<x>` on NixOS
hosts. The NixOS counterpart of a stow tree entry.

**Out-of-store symlink**:
An HM-managed home file produced via `config.lib.file.mkOutOfStoreSymlink`,
which targets a mutable path (`~/dotfiles/...`) instead of a derivation in
the Nix store. Edits to the source take effect without `nixos-rebuild switch`.
_Avoid_: live symlink, dev symlink.

**Host**:
A named NixOS configuration declared in `nixos/flake.nix`'s `hosts` attrset
(e.g. `fusion-vm`). Each host carries `system`, `modules`, and
`ci.{eval,build,runner}` metadata used by the flake-exported `lib.ciMetadata`.

**Platform tree**:
A top-level directory in the repo dedicated to one OS install path:
`nixos/`, `macos/`, `omarchy/`. The conventions for what belongs in each
are documented in `AGENTS.md`.

**Skill manifest**:
A proposed mirror for a platform that cannot directly enumerate
`common/skills/`. It is not an implemented root workflow: `make skills-sync`
and `make skills-sync-check` do not exist. Omarchy discovers skills directly;
see `docs/adr/0001-shared-skills-manifest.md` for implementation status.

## Relationships

- A **Skill** lives at `common/skills/<name>/`. The Omarchy installer exposes
  all such skills to every selected **Agent CLI**, without a second inventory.
- An **Agent CLI** is configured by a **Stow tree** on macOS / omarchy and
  by **HM-managed home files** on NixOS — two alternative materialization
  paths for the same logical config.
- A **Host** declares its `system` and imports `nixos/modules/common` plus
  per-host modules.
- The authoritative skill inventory is `common/skills/*/SKILL.md`; a
  **Skill manifest** would be derived from it, not independently authored.

## Flagged ambiguities

- "agent" was used for both **Agent CLI** (terminal application) and
  in-agent subagents. Resolved: in this repo, **Agent CLI** always refers
  to the user-facing terminal tool; intra-agent concepts use "subagent"
  or "skill invocation".
- "shared" was used loosely for "shared between hosts" vs. "shared between
  platforms". Resolved: **shared** means authoritative source under `common/`,
  not proof of installation on every host. Installation coverage is documented
  per platform.
