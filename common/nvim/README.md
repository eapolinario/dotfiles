# Shared Neovim configuration

Personal [LazyVim](https://www.lazyvim.org/) configuration. Keep this directory
flat: `init.lua`, `lazyvim.json`, and `lua/` are the shared source for every
platform, rather than maintaining a second Omarchy plugin tree.

## Platform behavior

`lua/config/platform.lua` detects macOS, the `/etc/NIXOS` marker, or the Omarchy
directory (`$OMARCHY_PATH`, defaulting to `~/.local/share/omarchy`). Merely having
`nix` installed does not select the NixOS profile.

| Platform | Tooling | Appearance and editor options |
|----------|---------|-------------------------------|
| Omarchy | Mason/default LazyVim management | Omarchy theme, transparency and remote clipboard; autoformat and relative numbers remain off |
| NixOS | Nix-provided servers, `nixd` and `nixfmt`; no Mason-managed Delve | Nordic; Snacks animations off |
| macOS | Homebrew `gopls`, Mason for other servers; Nix servers disabled | Nordic; Snacks animations off |
| Other Linux | Mason/default LazyVim management | Nordic; Snacks animations off |

The shared `lazyvim.json` enables 21 extras, including Telescope, Sidekick, Go
debugging, Overseer and Zig. Inline Git blame is enabled with a 300ms delay;
`<leader>uB` toggles it. Sidekick persists sessions with zellij when available,
otherwise tmux.

## Search bindings

Telescope provides the Doom-style directory searches (`<leader>.`,
`<leader>fD`, `<leader>sd`) and open-buffer grep (`<leader>sB`).

On Omarchy only, FFF retains `<leader><space>`, `<leader>ff`, `<leader>sg` and
`<leader>sw`. Their Telescope bindings are removed explicitly, including visual
mode word search. `<leader>sp` follows the same backend as `<leader>sg`.
Other pickers use Telescope. FFF is disabled on other platforms.

See the [Doom-to-Neovim cheatsheet](../../docs/doom-to-neovim-cheatsheet.md).

## Installing on Omarchy

From the repository root:

```sh
./omarchy/install.sh --nvim-only --dry-run
./omarchy/install.sh --nvim-only
```

The targeted command does not install other dotfiles or touch systemd services.
The full Omarchy installer also calls this migration.

Start from Omarchy's seeded Neovim configuration; if its support files are
missing, run `omarchy-nvim-setup` first. Installation backs up replaced regular
files and legacy repo-owned symlinks to a sibling `nvim.backup.*` directory,
then links shared files individually with Stow's `--no-folding`. Unrelated
symlinks and directory symlinks are rejected before any Neovim files are moved.
Re-running the installer does not create another backup when links are current.

**On Omarchy only**, the live config and plugin directories must remain real
directories. The installer leaves `lua/plugins/theme.lua`, `lazy-lock.json`,
and Omarchy's other support files local. These exclusions are explicit Stow
options; Git's ignore rules alone do not stop Stow from linking a file.

Omarchy setup leaves an existing config in place, but an explicit refresh
backs it up and replaces it. Re-run this installer after a refresh. Review
upstream changes to files now owned by the shared configuration; package
updates do not automatically merge them into the repo.

## Regression tests

From the repository root, using installed Neovim, Bash, GNU Stow and coreutils:

```sh
nvim --headless -u NONE -i NONE -l omarchy/tests/nvim-config.lua
bash omarchy/tests/nvim-install.sh
```

These exercise platform branches and migration in temporary directories,
without starting the normal Neovim configuration or changing live dotfiles.
