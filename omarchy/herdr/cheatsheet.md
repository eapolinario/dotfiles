# Herdr keybinding cheat sheet

Your Omarchy bindings from [config.toml](.config/herdr/config.toml), reviewed with
Herdr 0.9.5. The table includes every configured binding, useful retained defaults,
and mode controls—not the stock Herdr keymap.

**`P` = `Ctrl+Space`, release, then press the next key.** Uppercase letters mean
Shift; chords without `P` are pressed together. Mode-specific keys only apply
while that mode is open.

Think **workspace → tab → pane**, like tmux's session → window → pane.
Herdr's named persistent sessions are separate from workspaces.

| Context | Action | Shortcut(s) | Explanation |
| --- | --- | --- | --- |
| General | Prefix | `Ctrl+Space` | Enter prefix mode; release before the action key. Shown as `P` below. |
| General | Help | `P ?` | Show the active keymap; use the Help controls below to filter it. |
| General | Reload config | `P q` | Reload client settings and the selected server after config edits. Unlike stock Herdr, this does not detach. |
| General | Detach | `P d` | Leave Herdr while panes and agents keep running; use this instead of closing them. |
| General | Settings | `P s` | Open settings. Retained default binding. |
| General | Workspace navigation | `P w` | Enter persistent navigation mode; its workspace and pane controls are listed below. Retained default. |
| General | Go To picker | `P g` | Find and jump to a pane or agent, grouped by workspace. Retained default. |
| General | Toggle sidebar | `P b` | Collapse or expand the sidebar. Retained default. |
| General | Edit scrollback | `P e` | Open the focused pane's scrollback in an editor. Retained default. |
| General | Notification target | `P o` | Focus the target of the currently visible notification. Retained default. |
| Tabs | Create | `P c` | Create a tab in the current workspace, inheriting the pane's directory; no naming prompt. |
| Tabs | Rename | `P r` | Rename the current tab. Here `r` means rename, not resize mode. |
| Tabs | Close | `P k` | Kill the current tab and its panes/processes; this is not detach. |
| Tabs | Previous | `P p` or `Alt+←` | Switch to the previous tab. Lowercase `p` stays at tab level. |
| Tabs | Next | `P n` or `Alt+→` | Switch to the next tab. Lowercase `n` stays at tab level. |
| Tabs | Jump to 1–9 | `P 1..9` or `Alt+1..9` | Select a tab by index. **Ghostty intercepts Alt+digits** for its own tabs; use the prefixed numbers. |
| Tabs | Move left | `Alt+Shift+←` | Reorder the current tab toward the front; changes its position rather than selecting another tab. |
| Tabs | Move right | `Alt+Shift+→` | Reorder the current tab toward the back. |
| Workspaces | Create | `P C` | Create a workspace, inheriting the current directory. Shift promotes tab-create to workspace-create. |
| Workspaces | Rename | `P R` | Rename the workspace. Shift promotes tab-rename to workspace-rename. |
| Workspaces | Close | `P K` | Kill the workspace and its tabs/panes. **No confirmation** (`confirm_close = false`); this is not the stock pane-swap-up shortcut. |
| Workspaces | Previous | `P P` or `Alt+↑` | Switch to the previous workspace. `P P` means prefix, then Shift+p—not two prefixes. |
| Workspaces | Next | `P N` or `Alt+↓` | Switch to the next workspace. Shift promotes tab-next to workspace-next. |
| Workspaces | New Git worktree | `P G` | Create a Git worktree from the selected workspace. Retained default. |
| Panes | Split down | `P h` or `Alt+Enter` | **Horizontal divider → stacked panes.** H names the divider, opposite tmux's `split-window -h` convention. |
| Panes | Split right | `P v` or `Alt+Shift+Enter` | **Vertical divider → side-by-side panes.** V names the divider, opposite tmux's `split-window -v` convention. |
| Panes | Focus left | `Ctrl+Alt+←` | Focus the pane to the left. **Ghostty intercepts this** for its own splits; use workspace navigation for directional focus. |
| Panes | Focus down | `Ctrl+Alt+↓` | Focus the pane below. **Ghostty intercepts this** for its own splits; use workspace navigation instead. |
| Panes | Focus up | `Ctrl+Alt+↑` | Focus the pane above. **Ghostty intercepts this** for its own splits; use workspace navigation instead. |
| Panes | Focus right | `Ctrl+Alt+→` | Focus the pane to the right. **Ghostty intercepts this** for its own splits; use workspace navigation instead. |
| Panes | Cycle next | `P Tab` | Cycle forward through panes without the Ghostty arrow conflict. Retained default. |
| Panes | Cycle previous | `P Shift+Tab` | Cycle backward through panes. Retained default. |
| Panes | Last pane | `P ;` | Return to the last focused pane; useful for back-and-forth switching. |
| Panes | Zoom / unzoom | `P z` | Toggle the focused pane between the full tab area and its normal split layout. |
| Panes | Rename | `P O` | Give the focused pane a name; uppercase O means Shift+o. |
| Panes | Close | `P x` or `Alt+Esc` | Kill the focused pane/process, rather than leaving it running as detach does. |
| Panes | Copy mode | `P [` | Enter keyboard copy mode for the focused pane. Output remains live while you browse history. |
| Panes | Resize mode | `P Ctrl+←`, `P Ctrl+↓`, `P Ctrl+↑`, or `P Ctrl+→` | Any of these enters resize mode; the triggering arrow does not directly resize. Use the Resize controls below. |
| Panes | Resize left | `Ctrl+Alt+Shift+←` | Resize directly to the left; no prefix or resize mode needed. |
| Panes | Resize down | `Ctrl+Alt+Shift+↓` | Resize directly downward. Adding Shift to the pane-focus chord changes focus to resize. |
| Panes | Resize up | `Ctrl+Alt+Shift+↑` | Resize directly upward. |
| Panes | Resize right | `Ctrl+Alt+Shift+→` | Resize directly to the right. |
| Agents | Previous | `Alt+Shift+↑` | Follow the previous entry in agent-list order, not workspace or tab order. |
| Agents | Next | `Alt+Shift+↓` | Follow the next entry in agent-list order. |
| Agents | Jump to 1–9 | `P Alt+1..9` | Focus an agent by index. **Ghostty consumes Alt+digits even after a prefix**; use `P g` or the previous/next agent shortcuts instead. |
| Resize mode | Resize repeatedly | Arrows or `h/j/k/l` | Resize left/down/up/right while staying in the mode. |
| Resize mode | Exit | `Enter` or `Esc` | Return to normal terminal input. |
| Copy mode | Move | Arrows or `h/j/k/l` | Move the copy cursor left/down/up/right through the pane's history. |
| Copy mode | Next word | `w` | Move to the next word. |
| Copy mode | Previous word | `b` | Move to the previous word. |
| Copy mode | Word end | `e` | Move to the end of the word. |
| Copy mode | Page up | `PageUp` or `Ctrl+b` | Browse a full page backward through history. Ctrl+b is available because your prefix is Ctrl+Space. |
| Copy mode | Page down | `PageDown` or `Ctrl+f` | Browse a full page toward newer output. |
| Copy mode | Half-page up | `Ctrl+u` | Move half a page toward older output. |
| Copy mode | Half-page down | `Ctrl+d` | Move half a page toward newer output. |
| Copy mode | Search forward | `/`, query, `Enter` | Search literally forward; case-insensitive unless the query contains an uppercase letter. |
| Copy mode | Search backward | `?`, query, `Enter` | Search literally backward. |
| Copy mode | Repeat search | `n` | Find the next match in the same search direction. |
| Copy mode | Reverse search | `N` | Find the next match in the opposite direction. |
| Copy mode | Start selection | `v` or `Space` | Start selecting text from the copy cursor. |
| Copy mode | Copy selection | `y` or `Enter` | Copy selected text. Mouse drag-selection also copies by default. |
| Copy mode | Leave without copying | `q` or `Esc` | Exit copy mode; Esc clears an active selection or search before exiting. |
| Workspace navigation | Select workspace | `↑/↓` | Move through workspaces while navigation mode is open. |
| Workspace navigation | Focus pane | `←/→` or `h/j/k/l` | Left/right arrows focus horizontally; h/j/k/l focus left/down/up/right. Safe alternative to the intercepted Ctrl+Alt+arrows. |
| Workspace navigation | Exit | `Esc` | Leave navigation mode and return input to the focused pane. |
| Go To picker | Search | `/` | Search panes and agents by names, kinds, tabs, workspaces, branches, or paths. |
| Help | Filter | `/` | Filter the displayed actions and shortcuts. |
| Help | Edit filter | `Backspace` | Remove the last character from the filter. |
| Help | Clear filter | `Ctrl+u` | Clear the filter and show all bindings again. |

To enable the Ghostty-conflicting shortcuts, free those chords in Ghostty or
choose different Herdr bindings. `herdr config check` validates Herdr's config,
but cannot detect outer-terminal conflicts.

The installer links only `config.toml`; this cheat sheet stays in the repository.
See [Omarchy operations](../README.md) for installation and config reloads,
and [Herdr's keyboard guide](https://herdr.dev/docs/keyboard/) for mode behavior.
