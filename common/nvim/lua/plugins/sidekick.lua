-- folke/sidekick.nvim is enabled via the LazyVim sidekick extra (see lazyvim.json).
-- Persist AI CLI sessions across Neovim restarts, preferring zellij on NixOS
-- and falling back to tmux, which is installed on macOS.
local backend = vim.fn.executable("zellij") == 1 and "zellij" or "tmux"

local function send_shift_enter(terminal)
  local sequence = "\27[13;2u"
  if terminal.parent and terminal.parent.backend == "tmux" then
    local pane = assert(terminal.parent:pane_id(), "Sidekick tmux pane is not available")
    -- Sidekick's paste-buffer path escapes ESC; send the key bytes literally instead.
    local result = vim.system({ "tmux", "send-keys", "-l", "-t", pane, sequence }, { text = true }):wait()
    assert(result.code == 0, "Failed to send Shift+Enter to Sidekick: " .. (result.stderr or ""))
  else
    assert(terminal.job and terminal:is_running(), "Sidekick terminal is not running")
    vim.api.nvim_chan_send(terminal.job, sequence)
  end
end

return {
  {
    "folke/sidekick.nvim",
    opts = {
      cli = {
        win = {
          keys = {
            shift_enter = { "<S-CR>", send_shift_enter, desc = "Insert newline in AI CLI" },
            ghostty_shift_enter = { "<M-CR>", send_shift_enter, desc = "Insert newline in AI CLI" },
          },
        },
        mux = {
          backend = backend,
          enabled = true,
        },
      },
    },
  },
}
