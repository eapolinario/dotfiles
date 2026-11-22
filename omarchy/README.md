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

Default components are `doom`, `authinfo`, `hypr`, `ghostty`, `herdr-app`, `nvim`,
`copilot`, `claude`, `pi`, and `grasp`. Select a subset with a comma-separated list:

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
| Herdr app | Herdr desktop app and herdr-core (`hc`), plus `curl` for the pinned Panopticon release installer | `$XDG_CONFIG_HOME/herdr-app/`: `keys.json`, `settings.json`; Panopticon installed by its release installer |
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

Pi settings are Omarchy-specific: `omarchy/pi/.pi/agent/settings.json` is linked
at `~/.pi/agent/settings.json`, instead of the shared settings in `common/pi/`.
Shared Pi prompts, extensions, and discovered skills are still installed by
`--only pi`; credentials, sessions, and other runtime state stay local. Existing
links to the former shared settings file are backed up and migrated.

## Herdr desktop app

The desktop app's `keys.json` and `settings.json` live in
`omarchy/herdr-app/.config/herdr-app/`. Those files are linked individually;
the app directory stays real. Shipped layouts/mods, generated manuals/types,
telemetry, sockets, and session/window state remain local.

Panopticon is not vendored or Stowed. Selecting the `herdr-app` component downloads
and runs the project's official installer with `--ref v0.4.0` and
`--app-home <your-XDG-herdr-home>`. The pinned installer creates an
installer-managed `mods/panopticon/` directory containing only `main.js`, `mod.json`,
and the ownership marker. It preflights the resolved destination parent chain,
migrates legacy repository-owned Stow installs transactionally, and verifies planned
Stow links before changing the mod. An existing manual or symlinked Panopticon
directory is left alone and reported by the release installer as unmanaged. The installer does not reload or restart the app.

```sh
./omarchy/install.sh --only herdr-app --dry-run
./omarchy/install.sh --only herdr-app
herdr-app check "${XDG_CONFIG_HOME:-$HOME/.config}/herdr-app/mods/panopticon"
# With the desktop app running:
herdr-app do mod.open --name panopticon
herdr-app status
```

A dry-run lists the pinned Panopticon release and URL but does not download or
change the app home. Applying it requires `curl` and `herdr-app` on `PATH` and uses
network access; keys and existing runtime layouts are not edited by the upstream
installer. The downloaded installer script is removed after success and failed
transaction rollback. The normal dotfiles installer may also run the release update again
when `herdr-app` is selected, while the release installer itself updates only an
installer-managed copy.

**Panopticon** (`Ctrl+Shift+G`) switches the whole Herdr window to a grid of live
agent terminals from **all spaces**. It is a layout, not a space or tab. Idle agents
and live agent helpers are included; ordinary shells are not. Click inside a tile to
work in that terminal, or click its caption to go to the original pane (Peek for a
helper without a pane). `Ctrl+Shift+G` again or **Back** restores the previous layout.
`Esc` is still available to terminal programs; `Ctrl+Shift+K` remains the tab picker.

Panopticon creates its own `layouts/panopticon.json` in Herdr's local runtime directory
on first use. The layout contains session addresses and is **not** stowed or checked
in. Existing unrelated layouts of the same name are left untouched. Opening the grid
resizes terminal views to their tiles; it does not create, move or close sessions.

**herdr-nvim** ([upstream](https://github.com/ChmaraX/herdr-nvim/tree/main/herdr-app))
is installed separately from a reviewed, SHA-256-verified release bundle. Its
`herdr-nvim` and `herdr-nvim-header` mods and binary/Lua runtime stay local, not
stowed. **Alt+E** toggles Files and the tab's persistent Neovim, **Alt+O** picks a
file touched by an agent, and **Alt+Shift+E** focuses the tree. These shortcuts avoid
upstream's Super-key conflicts with Hyprland; existing split keys are unchanged.
The mod runs local programs and reads project files and agent session logs.

Installing on the shipped `main` layout creates and switches to `main-copy`;
`main` and Panopticon remain unchanged. Generic layout cycling is intentionally
unbound; `Ctrl+Shift+G` remains the Panopticon toggle.
Validate both mods with `herdr-app check <app-home>/mods/herdr-nvim` and
`herdr-app check <app-home>/mods/herdr-nvim-header`.

The v1.3.1 release installer atomically replaces `keys.json` when adding bindings.
Before running that installer, change its bundled `herdr-app/keys.json` to the
three Alt bindings above and add them to this repo's keys first, so its merge is
a no-op and preserves the live Stow symlink. Review key handling again on upgrades.

Leave `HERDR_APP_HOME` unset when using the standard installed directory; it
selects a different app config home and running window.

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

### Gazelle battery charge limit (system service, opt-in)

`systemd/system/battery-charge-limit.service` is specific to the System76 Gazelle
`gaze16-3050` running package-backed Omarchy. It stops charging at **70%** and resumes
below **60%**. This is a **system** service because the charge-control sysfs files
require root; it must not be installed as a user service.

The ordinary installer and `make install-omarchy` neither install nor activate this
unit. Its conditions require `/usr/bin/omarchy`, `BAT0`, and exact DMI matches for
vendor `System76` and product version `gaze16-3050`. Other models skip the service
before loading the driver or writing charge limits.

Apply explicitly on this Gazelle, from a terminal at the repository root:

```sh
rtk systemd-analyze verify --man=no omarchy/systemd/system/battery-charge-limit.service
rtk sudo install --backup=numbered -m 0644 \
  omarchy/systemd/system/battery-charge-limit.service \
  /etc/systemd/system/battery-charge-limit.service
rtk sudo systemctl daemon-reload
rtk sudo systemctl enable battery-charge-limit.service
rtk sudo systemctl restart battery-charge-limit.service
rtk head -n 1 /sys/class/power_supply/BAT0/charge_control_{start,end}_threshold
```

The last command should show `60` and `70`. The root-owned copy is deliberately
not a symlink to a user-writable checkout. After editing the tracked unit, repeat
the copy, daemon-reload, and restart commands; `install --backup=numbered` preserves
any previous unit in a numbered backup beside it.

This Gazelle's May 2024 firmware can lose custom thresholds after shutdown and
unplugging, so the service reapplies them at boot. See
[System76's battery-threshold documentation](https://system76.com/support/laptop-battery-thresholds/).
Setting a cap does not forcibly discharge a battery already above it.

To disable the boot service and restore the original 90/100 thresholds, set the
stop threshold first so it remains above the start threshold:

```sh
rtk sudo systemctl disable --now battery-charge-limit.service
rtk printf '100\n' | rtk sudo tee /sys/class/power_supply/BAT0/charge_control_end_threshold
rtk printf '90\n' | rtk sudo tee /sys/class/power_supply/BAT0/charge_control_start_threshold
```

Stopping or disabling the oneshot service alone does not reset firmware thresholds.

### Tailscale daemon (system service, opt-in)

The `tailscale` package provides the Arch package-backed `tailscaled.service`.
This repository only supplies a systemd drop-in that waits for network connectivity
and restarts a failed daemon after five seconds. It does not replace the package unit
or alter Tailscale state/socket/port settings.

The ordinary installer and `make install-omarchy` neither install the package nor
activate the service. Apply explicitly from a terminal at the repository root:

```sh
rtk sudo pacman -S --needed tailscale
rtk sudo install -D -m 0644 \
  omarchy/systemd/system/tailscaled.service.d/10-omarchy.conf \
  /etc/systemd/system/tailscaled.service.d/10-omarchy.conf
rtk sudo systemctl daemon-reload
rtk sudo systemctl enable --now tailscaled.service
```

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
