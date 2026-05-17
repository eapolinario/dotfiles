---
name: nixos-apply
description: >
  Commit pending changes in the dotfiles repo, push to origin, and apply them
  to the local NixOS machine with `make switch`. Use this skill whenever the
  user asks to "commit, push, and switch", "commit and apply", "deploy this
  config", "apply nixos changes", "save and rebuild", or any similar
  three-step "ship my NixOS edits" phrasing. Do not use for first-time
  installs (`make deploy` / `nixos-anywhere`) — this skill never wipes disks.
---

# nixos-apply

End-to-end "ship my NixOS edits" loop for Eduardo's `dotfiles` repo:

1. Commit staged + unstaged tracked changes with a meaningful message.
2. Push to `origin`.
3. Run `make switch` from `nixos/` to activate the new generation.

Always run from the repo root (`/home/eduardo/dotfiles`) unless the user
specifies otherwise.

---

## Step 1 — Inspect the change

```bash
cd /home/eduardo/dotfiles
rtk git status
rtk git diff
rtk git diff --staged
```

- If there are no modifications and no staged changes, ask the user whether
  they want to skip the commit/push and only run `make switch` (e.g. to
  re-apply after a manual `git pull`).
- Untracked files (`?`) are **not** included unless the user explicitly says
  so. Surface them in your summary so the user can decide.

---

## Step 2 — Validate (when nix files changed)

If any `*.nix` file under `nixos/` is in the diff, run validation first:

```bash
cd /home/eduardo/dotfiles/nixos && make fmt && make check
```

For pure documentation or non-nix tweaks (e.g. only files under `common/`,
`omarchy/`, `macos/`, or markdown), skip `make check` — it's slow and not
required.

If `make check` fails, stop, report the failure, and let the user decide
how to proceed. Do not commit broken config.

---

## Step 3 — Stage and commit

Stage only the files the user expects to ship (tracked modifications by
default). Prefer explicit paths over `git add -A` so untracked files don't
sneak in.

```bash
cd /home/eduardo/dotfiles
rtk git add <paths>
```

Write a short conventional-style commit message scoped to the area you
touched (e.g. `nushell:`, `hyprland:`, `nixos:`, `doom:`). Keep the subject
under 72 characters. Always include the `Co-authored-by: Copilot` trailer
per repo policy:

```bash
rtk git commit -m "<scope>: <subject>

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

If the user provides their own commit message, use it verbatim and still
append the trailer.

---

## Step 4 — Push

```bash
rtk git push
```

If the push is rejected (non-fast-forward, missing upstream, etc.), stop
and report — do not force-push without an explicit user request.

---

## Step 5 — Apply with `make switch`

```bash
cd /home/eduardo/dotfiles/nixos && make switch
```

The Makefile picks up `HOST` from the flake's `lib.ciMetadata.defaultBuildHost`
when unset, so a bare `make switch` works on a configured machine. If the
default host is empty (rare) or the user names a host, pass it explicitly:

```bash
cd /home/eduardo/dotfiles/nixos && make switch HOST=<name>
```

Available hosts live under `nixos/hosts/`.

`make switch` runs `sudo nixos-rebuild switch` and may prompt for the sudo
password — keep that in mind when running non-interactively.

---

## Step 6 — Report

Print a compact summary, e.g.:

```
✓ committed: nushell: enable case-insensitive completion matching
✓ pushed   : origin/main
✓ switched : fusion-vm (generation activated)
```

If any step was skipped (e.g. no changes to commit), say so explicitly in
the summary instead of silently dropping it.

---

## Notes & guardrails

- **Never** run `make deploy` or `nixos/deploy.sh` from this skill — those
  wipe disks via `nixos-anywhere`. This skill only activates an already
  installed system.
- **Never** force-push, rewrite history, or commit decrypted `git-crypt`
  contents.
- If `common/authinfo/.authinfo` or `gitleaks.baseline` are part of the
  diff, run `make gitleaks` from the repo root before pushing.
- Use `rtk` prefixes on git/gh commands per repo convention to keep output
  compact.
- If the user is on a non-NixOS machine (e.g. macOS), this skill does not
  apply — surface that and stop after the push, or hand off to the matching
  platform workflow.
