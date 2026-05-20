-- LSP servers managed by Nix (home.packages), not Mason.
-- Add a server here when you install its binary via nixpkgs.
local flake = vim.fn.expand("~/dotfiles/nixos")
local host = vim.loop.os_gethostname()

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        gopls = {
          mason = false,
        },
        marksman = {
          mason = false,
        },
        -- nil_ls comes from the lang.nix LazyVim extra; we use nixd instead.
        nil_ls = { enabled = false },
        nixd = {
          mason = false,
          settings = {
            nixd = {
              nixpkgs = {
                expr = string.format('import (builtins.getFlake "%s").inputs.nixpkgs { }', flake),
              },
              options = {
                nixos = {
                  expr = string.format('(builtins.getFlake "%s").nixosConfigurations.%s.options', flake, host),
                },
                ["home-manager"] = {
                  expr = string.format(
                    '(builtins.getFlake "%s").nixosConfigurations.%s.options.home-manager.users.type.getSubOptions []',
                    flake,
                    host
                  ),
                },
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
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = {} },
  },

  -- LazyVim's tool installer; keep empty so it doesn't fight Nix either.
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = { ensure_installed = {} },
  },

  -- lang.nix extra wires up statix via nvim-lint; we don't install it.
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        nix = {},
      },
    },
  },

  -- lang.nix extra defaults to alejandra; prefer nixfmt (already in nixpkgs).
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        nix = { "nixfmt" },
      },
    },
  },
}
