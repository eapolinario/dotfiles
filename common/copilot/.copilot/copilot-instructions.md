# Global Copilot instructions

Loaded into every Copilot CLI session on this machine, in every repository. A
repo's own `AGENTS.md` or `.github/copilot-instructions.md` wins on conflict.

Keep this file short: it costs context in every session.

## Command output: prefix with `rtk`

`rtk` is a CLI proxy that filters and compresses command output (60-90% fewer
tokens). It passes commands through unchanged when it has no dedicated filter,
so it is always safe to prefix.

- Prefix shell commands with `rtk`, including inside `&&` chains:
  `rtk git add -A && rtk git commit -m "..."`, not `git add -A && git commit ...`.
- Highest value: `rtk git status|log|diff|show`, `rtk gh pr view|checks`,
  `rtk gh run list`, `rtk err <cmd>` (errors only), `rtk test <cmd>` (failures
  only), `rtk curl`, `rtk docker`, `rtk kubectl`.
- `rtk proxy <cmd>` bypasses filtering when the raw output actually matters.
- Exception: to read or search files, use the built-in `view`, `grep`, and
  `glob` tools rather than `rtk read|grep|ls`. They are cheaper still.

## Tools

- Prefer built-in `view`/`grep`/`glob` over `cat`, `rg`, and `find`. Reach for
  bash only when no built-in covers the job.
- Python: run everything through `uv` (`uv run`, `uv add`). Never `pip install`
  into a system interpreter.
- GitHub: use `rtk gh ...` rather than hand-rolled `curl` against the API.

## Git

- Commit subjects are `<scope>: <imperative summary>` (`omarchy: add nvim stow
  package`) or Conventional Commits (`feat(omarchy): ...`, `fix(doom): ...`).
  Under 72 characters.
- Never rewrite published history. Never commit secrets or decrypted files.
- For parallel work across issues, use worktrees at
  `~/repos/<repo>.worktrees/<issue-branch>` instead of switching branches in
  place. Give each background agent its own worktree and tell it to stay there.

## Evidence

- Cite the file, line, doc URL, or command output backing a factual claim.
  Say "I could not verify this" instead of filling a gap with a plausible guess.
- Version numbers, release dates, and API shapes drift. Check them against the
  source rather than recalling them.
- When a conclusion needs an independent check, spawn a fresh subagent with the
  relevant context instead of re-defending the original answer in place.
- Never report work as done without running the build, test, or command that
  proves it.

## Touching the live system

- Most of `~/.config` is GNU stow symlinks into `~/repos/dotfiles`. Edit the
  file inside the repo, never the symlink target in `$HOME`, and never replace
  a stowed symlink with a regular file.
- Prefer a `--dry-run` pass first for anything that mutates the machine.
- `sudo`, service restarts, `nixos-rebuild switch`, and destructive `nix`
  operations are opt-in: ask before running them.

## Shell scripts

- `#!/usr/bin/env bash` and `set -euo pipefail`.
- `[[ ... ]]` over `[`, `printf` over `echo`, quote every expansion.
- `lower_snake_case` for functions and variables, `UPPER_SNAKE_CASE` for
  readonly constants.
- Validate touched scripts with `bash -n`, `shellcheck`, and `shfmt -d`.
