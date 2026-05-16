-- LSP servers managed by Nix (home.packages), not Mason.
-- Add a server here when you install its binary via nixpkgs.
return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        gopls = {
          mason = false,
        },
        nixd = {
          mason = false,
          settings = {
            nixd = {
              nixpkgs = {
                expr = "import <nixpkgs> { }",
              },
              formatting = {
                command = { "nixfmt" },
              },
            },
          },
        },
      },
    },
  },

  -- Prevent mason-lspconfig from auto-installing servers we get from Nix.
  {
    "williamboman/mason-lspconfig.nvim",
    opts = { ensure_installed = {} },
  },

  -- LazyVim's tool installer; keep empty so it doesn't fight Nix either.
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = { ensure_installed = {} },
  },
}
