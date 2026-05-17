# Doom Emacs to Neovim Cheatsheet

This compares the Doom Emacs bindings in `common/doom` with the LazyVim-based
Neovim setup in `common/nvim`.

Assumptions:

- Doom leader: `SPC`
- LazyVim leader: `<Space>`
- Neovim uses LazyVim's Telescope extra.
- Neovim uses LazyVim's Sidekick extra, with this repo opting into Sidekick's
  zellij mux backend in `common/nvim/lua/plugins/sidekick.lua`.
- Repo-specific Doom bindings are marked as custom.

## Search and Files

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Find file in project | `SPC p f` or `SPC SPC` | `<Space><Space>` or `<Space>ff` |
| Find file from current directory | `SPC f f` | `<Space>fF` |
| Find config file | `SPC f p` / Doom config helpers | `<Space>fc` |
| Recent files | `SPC f r` | `<Space>fr` |
| Switch buffer | `SPC b b` | `<Space>,` or `<Space>fb` |
| Search project text | `SPC /` or `SPC s p` | `<Space>/` or `<Space>sg` |
| Search current directory | `SPC s d` / consult from cwd | `<Space>sG` |
| Search current word | `SPC s s` / symbol search | `<Space>sw` |
| Resume last search | consult history / minibuffer history | `<Space>sR` |
| Search keybindings | `SPC h b b` or `C-h B` | `<Space>sk` |
| Search help | `SPC h f`, `SPC h v`, `SPC h d h` | `<Space>sh` |

## Telescope Basics

| Task | Neovim / Telescope |
|------|--------------------|
| Move down/up | `<C-n>` / `<C-p>` |
| Open selected result | `<Enter>` |
| Open in horizontal split | `<C-x>` |
| Open in vertical split | `<C-v>` |
| Open in tab | `<C-t>` |
| Toggle selection | `<Tab>` |
| Close picker | `<Esc>` or `<C-c>` |
| Show picker actions | `<C-/>` in insert mode, `?` in normal mode |

## Buffers, Windows, and Tabs

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Next buffer | `] b` | `<S-l>` or `]b` |
| Previous buffer | `[ b` | `<S-h>` or `[b` |
| Kill/delete buffer | `SPC b k` | `<Space>bd` |
| Split below | `SPC w s` | `<Space>-` |
| Split right | `SPC w v` | `<Space>|` |
| Close window | `SPC w c` | `<Space>wd` |
| Move to left window | `SPC w h` | `<C-h>` |
| Move to lower window | `SPC w j` | `<C-j>` |
| Move to upper window | `SPC w k` | `<C-k>` |
| Move to right window | `SPC w l` | `<C-l>` |
| New workspace/tab | `SPC TAB n` | `<Space><Tab><Tab>` |
| Next workspace/tab | `SPC TAB ]` | `<Space><Tab>]` |
| Previous workspace/tab | `SPC TAB [` | `<Space><Tab>[` |
| Close workspace/tab | `SPC TAB d` | `<Space><Tab>d` |

## Git

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Git status | `SPC g s` custom, or `SPC g g` | `<Space>gs` |
| Git commits | Magit log from status | `<Space>gc` or `<Space>gl` |
| Git stash | Magit stash from status | `<Space>gS` |
| Git blame current line | Magit blame / Doom git bindings | `<Space>gb` |
| Git current file history | Magit file log | `<Space>gf` |
| Copy remote URL for commit | `Y` in Magit log custom | No direct default |
| Create Git link | `C-c v &` custom | No direct default |

## LSP and Code

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Go to definition | `gd` | `gd` |
| Go to declaration | `gD` | `gD` |
| Go to implementation | Doom LSP lookup binding | `gI` |
| Go to type definition | Doom LSP lookup binding | `gy` |
| Find references | `SPC r` custom in prog buffers | `gr` |
| Hover docs | `K` | `K` |
| Signature help | LSP signature binding | `gK` or `<C-k>` in insert mode |
| Rename symbol | `SPC c r` | `<Space>cr` |
| Code action | `SPC c a` | `<Space>ca` |
| Source action | Doom code/action menu | `<Space>cA` |
| Format buffer | `SPC c f` or format-on-save | `<Space>cf` |
| LSP info | Doom LSP menu | `<Space>cl` |
| Split comma-separated list | `SPC c S` custom in prog buffers | No direct default |

## Terminals and Commands

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Open command palette | `SPC SPC` / `M-x` depending context | `:` or `<Space>sC` for commands |
| Command history | minibuffer history | `<Space>:` or `<Space>sc` |
| Open terminal at project root | `SPC o e` custom Eat terminal | `<C-/>` or `<Space>ft` |
| Open terminal in current directory | `SPC o E` custom other-window terminal | `<Space>fT` |
| Escape insert mode | `fd` custom Evil escape | `<Esc>` |

## Sidekick AI CLI

Sidekick runs AI CLI tools such as Claude, Codex, Copilot, and Gemini in a
Neovim terminal pane. This repo enables Sidekick through LazyVim and stores
sessions with zellij when available.

| Task | Neovim / LazyVim |
|------|------------------|
| Toggle Sidekick CLI pane | `<Space>aa` |
| Focus Sidekick CLI pane | `<C-.>` from normal, insert, terminal, or visual mode |
| Select AI CLI tool | `<Space>as` |
| Detach current CLI session | `<Space>ad` |
| Send current file | `<Space>af` |
| Send current line/object/context | `<Space>at` |
| Send visual selection | Select text, then `<Space>av` |
| Select and send prompt | `<Space>ap` |
| Use Next Edit Suggestion | `<Tab>` in normal mode |
| Toggle Sidekick NES | `<Space>uN` |

### Sidekick Pane Navigation

The Sidekick pane has three useful navigation contexts:

- **Terminal mode**: keys are sent to the AI CLI prompt.
- **Zellij scroll mode**: keys move through zellij's scrollback, the same
  history the mouse wheel can reach.
- **Neovim terminal normal mode**: keys move through Neovim's visible terminal
  buffer only.

With this repo's zellij backend, use **zellij scroll mode** for real history
navigation. `<C-q>` only enters Neovim terminal normal mode, so it can feel
stuck on the current visible page.

| Task | Key |
|------|-----|
| Focus Sidekick from an editor window | `<C-.>` |
| Enter zellij scroll mode from the Sidekick terminal | `<C-s>` |
| Move one line in zellij scroll mode | `j` / `k` or Down / Up |
| Move half a page in zellij scroll mode | `d` / `u` |
| Move a full page in zellij scroll mode | PageDown / PageUp |
| Search zellij scrollback | `s`, type search, `<Enter>`, then `n` / `p` |
| Return to live CLI output from zellij scroll/search mode | `<C-c>` |
| Enter Neovim terminal normal mode, visible page only | `<C-q>` |
| Return to typing in the AI CLI from Neovim normal mode | `i` or `a` |
| Send Enter to the CLI from Neovim normal mode | `<Enter>` |
| Hide Sidekick from normal mode | `q` or `<C-q>` |
| Hide Sidekick from terminal mode | `<C-.>` |
| Return to previous window without hiding Sidekick | `<C-z>` |
| Move from a right-side Sidekick split back to the editor | `<C-h>` |
| Move between Sidekick split windows when available | `<C-h/j/k/l>` |
| Open Sidekick buffer picker | `<C-b>` |
| Open Sidekick file picker | `<C-f>` |
| Insert a saved prompt or context | `<C-p>` |

Inside the Sidekick pane, `<C-f>` and `<C-b>` are reserved for Sidekick's file
and buffer pickers, so prefer `d`, `u`, PageDown, and PageUp for scrolling.

`<C-h/j/k/l>` are Sidekick window-navigation keys in terminal mode, not output
scrolling keys. They only navigate when the CLI is in a non-floating layout and
there is another Neovim window in that direction. With the default right-side
split, `<C-h>` is the common way back to the editing window.

Quick loop:

1. `<C-.>` to focus Sidekick.
2. `<C-s>` to enter zellij scroll mode.
3. Use `j`, `k`, `u`, `d`, PageUp, PageDown, or `s` search.
4. Press `<C-c>` to return to the live AI CLI output.

## Org Mode

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Agenda | `SPC o A` / Doom Org bindings | No direct default |
| Capture | `SPC X` / Doom Org capture bindings | No direct default |
| Archive done tasks | `SPC t A` custom in Org buffers | No direct default |
| Next sibling and hide current | `SPC ] ]` custom in Org buffers | No direct default |
| Previous sibling and hide current | `SPC [ [` custom in Org buffers | No direct default |

## Embark / Actions

| Task | Doom Emacs | Neovim / LazyVim |
|------|------------|------------------|
| Act on current completion/item | `M-/` custom Embark act | Telescope picker actions with `<C-/>` or `?` |
| Do what I mean | `C-;` custom Embark DWIM | Context-specific LazyVim mappings |
| Show action bindings | `C-h B` custom Embark bindings | `<Space>sk`, or Telescope `?` inside picker |

## Quick Translation Rules

- Doom `SPC p ...` project commands usually map to LazyVim `<Space>f...` or
  `<Space>s...` Telescope commands.
- Doom `SPC s ...` search commands usually map to LazyVim `<Space>s...`.
- Doom `SPC b ...` buffer commands usually map to LazyVim `<Space>b...`, with
  `<Space>,` as the fastest buffer switcher.
- Doom `SPC w ...` window commands mostly map to LazyVim `<Space>w...`, but
  movement is faster with `<C-h/j/k/l>`.
- Doom Magit workflows mostly start at `SPC g s`; LazyVim's Git pickers are
  spread under `<Space>g...`.
