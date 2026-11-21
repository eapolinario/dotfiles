---
name: dotfiles-commit
description: >
  Review, validate, stage, and commit requested changes in this dotfiles repository
  using its scoped-message, explicit-staging, and co-author conventions. Use when
  the user asks to commit changes here or invokes /skill:dotfiles-commit. Ordinary
  edits, or creating/editing this skill, do not authorize a commit. Push or apply
  configuration only when explicitly requested.
---

# dotfiles-commit

Keep this workflow local to the `dotfiles` repository. Read
[`../../../AGENTS.md`](../../../AGENTS.md) first; it is authoritative for platform
scope, validation, and safety. Determine the current checkout root with
`rtk git rev-parse --show-toplevel`; do not hardcode a home directory or repo path.
Run commands from that root unless a platform check specifies another directory.
Prefix shell commands with `rtk`, including each command in a chain.

## 1. Inspect and establish scope

```bash
rtk git status --short
rtk git diff --stat
rtk git diff --cached --stat
rtk git log -10 --oneline
```

- Review unstaged and staged changes for the intended non-secret paths with
  `rtk git diff --no-textconv -- <paths>` and
  `rtk git diff --cached --no-textconv -- <paths>`. Read intended new files too.
  Never display authinfo or credential contents.
- Commit the user's requested change, not every dirty file. Include its intended
  new files, but do not sweep in unrelated untracked files or runtime state.
  Ask if the requested scope or ownership of an edit is unclear.
- If unrelated changes are already staged, stop and ask how to handle them;
  an ordinary commit includes the whole index. Do not silently unstage user work.
- Keep each commit coherent. Include the configuration, installer plumbing,
  documentation, and tests needed for that change together. Separate unrelated
  concerns rather than making one commit per file.
- If there is nothing in scope to commit, report that; do not create an empty commit.

## 2. Validate the intended changes

Use the change-area checks in `AGENTS.md`, not every platform's checks:

| Change area | Validation |
|-------------|------------|
| Touched shell scripts | `rtk bash -n <file>`, `rtk shellcheck <file>`, `rtk shfmt -i 2 -ci -d <file>` |
| Omarchy installer or stow-managed config | `rtk ./omarchy/install.sh --dry-run` |
| Omarchy behavior or shared editor/agent config | `rtk make check-omarchy` |
| NixOS Nix config | From `nixos/`: `rtk make fmt && rtk make check` |
| macOS installer or Brewfile | On macOS, from `macos/`: `rtk brew bundle check` |
| Secrets-related files | `rtk make gitleaks` |
| Documentation or repo-local skills only | `rtk git diff --check`; for skills, validate frontmatter and discovery |

- Use available runtime-specific validators when relevant, without installing
  applications just to run a check. Do not run live installers, service activation,
  `make switch`, or deployment as validation.
- Inspect formatter changes and keep unrelated reformatting out of the commit.
- If a check fails or cannot run, report the exact limitation. Stop before committing
  unless the user explicitly accepts that failure or missing check. A narrower
  successful check does not make a failed full check pass.
- `make gitleaks` scans committed history, not the pending diff. Review intended
  files for secrets before staging, ensure encrypted files retain their git-crypt
  protection, and rerun the history scan after a secrets-related commit, before
  any requested push. Never regenerate the baseline just to silence a finding.

## 3. Stage deliberately and review the index

```bash
rtk git add -- <explicit-paths>
rtk git diff --cached --check
rtk git diff --cached --stat
```

Review the staged non-secret content again with
`rtk git diff --cached --no-textconv -- <paths>`. Confirm that the index contains
only the intended commit, including necessary new files, and that validation
covers its final contents. Stage only intended hunks in mixed files; ask if that
cannot be done safely without disturbing existing staged work.

Do not use blanket `git add .`, `git add -A`, or `git commit -a`. Do not discard,
stash, or reset unrelated changes to manufacture a clean worktree.

## 4. Use the repository's message conventions

- Prefer a short, imperative `<scope>: <subject>` with a subject under 72 characters
  and no trailing period. Choose the relevant component or platform, such as
  `doom`, `nvim`, `pi`, `hypr`, `omarchy`, `nixos`, `macos`, `security`, or `docs`.
- History also uses `feat(scope):`, `fix(scope):`, `docs(scope):`, and
  `refactor(scope):`; use these when appropriate, not as a mandatory release format.
- Examples: `pi: enable subagents on Omarchy`,
  `omarchy: manage Herdr config in dotfiles`, and
  `docs(skills): add a repo-local commit workflow`.
- Add a short body only when useful: explain why, a non-obvious tradeoff, or checks
  actually run. Do not invent issue references or append squash-merge PR numbers.
- Preserve a user-supplied message verbatim. Follow the existing agent-commit
  trailer convention documented in `common/skills/nixos-apply/SKILL.md`, unless
  the user explicitly requests different attribution:

```text
Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>
```

Append the trailer if absent; do not duplicate it. Do not invent model identities,
`Copilot-Session` IDs, sign-offs, or additional co-authors. Leave Git's configured
author and signing behavior alone.

## 5. Commit and report

For a commit without a body:

```bash
rtk git commit -m "<scope>: <subject>" \
  -m "Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
rtk git show --stat --oneline HEAD
rtk git status --short
```

If a hook, signing step, or commit fails, stop and report it; do not bypass checks,
rewrite history, or retry with broader staging. Never amend or rebase unless asked.

Report the commit hash and subject, validation results or accepted limitations,
and any remaining changes. Do not imply the worktree is clean without checking.

Pushing is a separate action: only run `rtk git push` when the user requested it.
Honor the configured branch/upstream. On a rejection or missing upstream, stop
and report; do not force-push, pull/rebase, or change branches automatically.
Committing or pushing does not authorize applying configuration or deploying.
