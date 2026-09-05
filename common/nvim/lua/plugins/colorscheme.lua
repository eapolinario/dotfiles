-- Omarchy's generated plugins/theme.lua owns its colorscheme and integrations.
if require("config.platform").omarchy then
  return {}
end

-- Use Nordic as the LazyVim colorscheme.
-- Catppuccin is kept around as an alternative; LazyVim already ships a
-- catppuccin plugin spec with a curated set of integrations (snacks, flash,
-- neotree, telescope, ...) and we only override the bits we care about so
-- those integrations are preserved.
return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = false,
    opts = {
      transparent_background = true,
    },
  },

  {
    "AlexvZyl/nordic.nvim",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require("nordic").setup(opts)
    end,
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "nordic",
    },
  },
}
