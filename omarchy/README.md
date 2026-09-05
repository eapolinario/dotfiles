# Omarchy operations

These files target Omarchy 4's Lua-based Hyprland configuration. The installer
links configuration; it does not install Omarchy, Doom, agent CLIs, fonts, or
language servers. Run commands below from the repository root.

## Preview and select components

```sh
./omarchy/install.sh --help
./omarchy/install.sh --doctor
./omarchy/install.sh --dry-run
./omarchy/install.sh
```

Both `--doctor` and `--dry-run` are read-only: no directories, backups, downloads,
symlinks, or service-state changes. Doctor reports missing runtimes with a
nonzero exit status; it checks availability, not network access or provider
authentication. A dry-run preflights the selected source/target paths and prints
the planned Stow commands. It does not conceal errors with `|| true`.

Default components are `doom`, `authinfo`, `hypr`, `ghostty`, `nvim`, `copilot`,
`claude`, `pi`, and `grasp`. Select a subset with a comma-separated list:

```sh
./omarchy/install.sh --only doom,copilot --dry-run
./omarchy/install.sh --only doom,copilot
./omarchy/install.sh --nvim-only
```

`--nvim-only` remains an alias for `--only nvim` and rejects service options.
An ordinary installation **does not enable, start, stop, or reload services**.
The `grasp` component only installs its unit unless `--enable-services` is
explicitly requested. `downloads-cleanup` is never selected by default.

## Prerequisites and destinations

The installer requires Bash, GNU Stow, GNU coreutils, and findutils.

| Component | Runtime prerequisites / assumptions | Destination |
|-----------|-------------------------------------|-------------|
| Doom | Emacs and a separately bootstrapped Doom; `doom sync` after module/package changes | `$XDG_CONFIG_HOME/doom` |
| Hyprland | Omarchy Lua defaults; `hyprctl`, `jq`, `zenity` for workspace swapping | `$XDG_CONFIG_HOME/hypr` |
| Ghostty | Ghostty and the configured Nerd Font; Omarchy theme file is optional | `$XDG_CONFIG_HOME/ghostty` |
| Neovim | Neovim/LazyVim; Omarchy's seeded clipboard support when on Omarchy | `$XDG_CONFIG_HOME/nvim` |
| Agents | Their respective CLI; Claude's configured command hook also needs `rtk` | `~/.copilot`, `~/.claude`, `~/.pi/agent` |
| Authinfo | An unlocked repository to install credentials | `~/.authinfo` |
| Grasp | A user systemd manager, `uvx` on its PATH, and `~/org` | `$XDG_CONFIG_HOME/systemd/user` |
| Downloads cleanup | Explicit opt-in; systemd-tmpfiles and a user systemd manager to activate it | Unit plus a private local rule |

`XDG_CONFIG_HOME` defaults to `~/.config` and must be absolute. A custom path is
used for installation **and** Hyprland's workspace-script binding. Applications
and the systemd user manager must use the same XDG environment; this installer
does not change or import your session environment. Moving the config home does
not automatically remove links from the previous destination.

The selected agent components discover every `common/skills/*/SKILL.md` and link
those skill directories individually. Adding a shared skill requires no
Omarchy manifest update. Unrelated local skills, logs, session databases, and
other runtime state are left alone. Tracked settings files are backed up and
replaced, not JSON-merged.

Missing/locked authinfo is skipped with a warning, without replacing existing
credentials. Use `--require-secrets` to make that a preflight failure:

```sh
./omarchy/install.sh --only authinfo --require-secrets --dry-run
```

The installer never unlocks the repository or prints credential contents. See
[the authinfo guide](../common/authinfo/README.md).

## Explicit service activation

For Grasp, provision the runtime and capture directory yourself, then:

```sh
./omarchy/install.sh --only grasp --doctor
./omarchy/install.sh --only grasp --enable-services --dry-run
./omarchy/install.sh --only grasp --enable-services
systemctl --user status grasp.service --no-pager
journalctl --user -u grasp.service -n 50 --no-pager
```

Grasp writes `~/org/capture.org`. Its first launch through `uvx` may download the
backend; the installer and doctor never do. Install/configure the
[browser extension](https://github.com/karlicoss/grasp) separately.

### Downloads cleanup is destructive

**Cleanup permanently deletes contents; it does not move them to Trash.**
The default retention is `7d`, using systemd-tmpfiles' age semantics rather than
just modification time. It runs when the selected service starts, including
login and an explicit `--enable-services` installation. No timer is installed.

```sh
# Install the rule/unit without starting cleanup.
./omarchy/install.sh --only downloads-cleanup --downloads-age 30d --dry-run
./omarchy/install.sh --only downloads-cleanup --downloads-age 30d

# Starting it may immediately delete eligible contents.
./omarchy/install.sh --only downloads-cleanup --downloads-age 30d --enable-services
```

Alternatively, `--with-downloads-cleanup` adds cleanup to another selection.
Use `--downloads-dir /absolute/path` for a dedicated alternate directory.
Home/config/repository roots and overlapping protected paths are rejected, as
are glob/control characters, including those introduced by symlink resolution.
The directory need not already exist; the tmpfiles `e` rule does not create it.

`--downloads-age 0` explicitly requests deletion of **all contents**. Supported
units are `s`, `min`, `h`, `d`, and `w`. Repeat custom settings when reinstalling
this component; omitted options use the documented defaults.

The generated rule is a regular, local file at
`$XDG_CONFIG_HOME/dotfiles/downloads-cleanup.conf`, outside `user-tmpfiles.d`.
The dedicated service reads only that rule, so it does not run unrelated user
cleanup rules and a generic user cleanup sweep cannot discover this one.

For upgrades, the old tracked `empty-downloads.conf` is now a harmless
compatibility placeholder. Existing service enablement is not silently removed;
the updated unit skips startup until its private rule exists. To stop either
service intentionally, use the appropriate explicit operator command:

```sh
systemctl --user disable --now downloads-clean-at-login.service
# Or, independently:
systemctl --user disable --now grasp.service
```

## Backups, conflicts, and recovery

All selected file paths are preflighted before the first mutation. Existing
regular files are backed up, even if identical, before being replaced by links.
Foreign symlinks and directory conflicts are refused, rather than adopted.
Legacy repo-owned folded *static subdirectories* (for example Doom's
`org-templates`) are migrated transactionally. Neovim and agent config/state
directories must stay real directories; their directory symlinks are rejected.

Backups are created only when needed, normally beside the component directory:
`doom.backup.*`, `nvim.backup.*`, `~/.copilot.backup.*`, etc. Authinfo uses
`~/.authinfo.backup.*`; Grasp uses `$XDG_CONFIG_HOME/systemd/grasp.backup.*`;
cleanup uses `$XDG_CONFIG_HOME/dotfiles/downloads-cleanup.backup.*`.
The installer prints each location. `RESTORE.tsv` maps each original absolute
target to its path within that backup. Backup directories are private to the
user, and symlinks are saved as links rather than dereferenced.

On an ordinary file-installation failure or interrupt, the installer removes
its new links and restores moved entries. It refuses to overwrite a path that
changed unexpectedly during rollback and reports the preserved backup instead.
This is not a power-loss transaction: keep the printed backup locations until
the configuration is working.

For manual restoration, inspect `RESTORE.tsv`, move the current target aside,
then move the corresponding backed-up entry back to its original path. Do not
write file contents through a managed symlink. For an unfolded directory,
restore the saved directory link as a whole, not individual files through it.
Do not delete backups until their contents have been reviewed.

Service activation happens **after** the file transaction and cannot be rolled
back (particularly deletion). An activation error reports that files remain
installed; inspect `systemctl --user status`, the journal, and enablement before
retrying. Rerunning with unchanged options/links creates no extra backups.

## Updating and regression coverage

Keep changes in this repository rather than replacing Stow links in `$HOME`.
For Neovim refreshes, generated theme/lockfile ownership, and migration details,
follow [the Neovim guide](../common/nvim/README.md). Other upstream Omarchy
defaults are not automatically merged into these tracked overrides.

```sh
make check-omarchy
make test-omarchy
```

The suites use fixture homes, mocked desktop/service commands, headless Neovim
without normal configuration, and built-in Emacs ERT. They do not bootstrap
plugins, start services, or move real desktop windows. Files directly under
`omarchy/tests/` are standalone suites; put supporting fixtures in subdirectories.
CI uses the same Make target. See [coverage and limitations](../docs/codebase/TESTING.md).
