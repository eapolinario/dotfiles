-- folke/sidekick.nvim is enabled via the LazyVim sidekick extra (see lazyvim.json).
-- Persist AI CLI sessions across Neovim restarts, preferring zellij on NixOS
-- and falling back to tmux, which is installed on macOS.
local backend = vim.fn.executable("zellij") == 1 and "zellij" or "tmux"

return {
  {
    "folke/sidekick.nvim",
    opts = {
      cli = {
        mux = {
          backend = backend,
          enabled = true,
        },
      },
    },
  },
}
