-- folke/sidekick.nvim is enabled via the LazyVim sidekick extra (see lazyvim.json).
-- Opt into the zellij mux backend so AI CLI sessions (Claude, Copilot, ...) persist
-- across Neovim restarts. zellij and lsof come from nixos/home/eduardo/default.nix.
return {
  {
    "folke/sidekick.nvim",
    opts = {
      cli = {
        mux = {
          backend = "zellij",
          enabled = true,
        },
      },
    },
  },
}
