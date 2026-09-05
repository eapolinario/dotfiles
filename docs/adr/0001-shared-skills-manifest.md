# Shared skills: discovery and materialization

## Status

**Implemented for the Omarchy installer; the cross-platform manifest design is
a proposal, not an implemented root workflow.**

An earlier version of this ADR described `make skills-sync`,
`make skills-sync-check`, and their CI enforcement as complete. Those root
targets do not exist. Do not use this document as evidence that a generated
NixOS manifest or cross-platform drift check is operational.

## Decision implemented on Omarchy

`common/skills/<name>/SKILL.md` is the source of truth for a shared skill.
At installation time, `omarchy/install.sh` discovers these files and links
their containing directories into each selected agent's skill directory:

- Copilot: `~/.copilot/skills`
- Claude: `~/.claude/skills`
- pi: `~/.pi/agent/skills`

The agent configuration/state directories stay real directories. Unrelated
local skills and runtime files are preserved; foreign conflicting links fail
preflight. The older tracked Claude/Copilot skill aliases are retained for
existing platform consumers but are not Omarchy's inventory.

Adding a skill on Omarchy requires creating its `SKILL.md`, then rerunning the
selected agent installation. A directory without that file is not a skill.
`omarchy/tests/install.sh` exercises discovery across all three agents,
including a skill added after the first installation. Omarchy CI runs it.

## Deferred platform design

The original proposal was to mirror the shared directory listing into the
NixOS flake tree, avoiding parent-directory reads during pure evaluation.
Alternatives included moving the flake to the repository root, introducing a
parent path input, or allowing impure evaluation.

Any implementation or change to that approach requires a separate NixOS-scoped
review of its actual materialization path and local/CI commands. Likewise, do
not assume the macOS installer has Omarchy's discovery behavior merely because
both consume shared sources.

## Evidence

- `omarchy/install.sh`: `add_agent` and component selection
- `omarchy/tests/install.sh`: shared-skill parity and discovery fixtures
- `Makefile`: available root workflows
- `.github/workflows/omarchy-ci.yml`
- `common/skills/`
