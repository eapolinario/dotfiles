-- Use Catppuccin as the LazyVim colorscheme.
-- LazyVim already ships a catppuccin plugin spec with a curated set of
-- integrations (snacks, flash, neotree, telescope, ...). We only override
-- the bits we care about so those integrations are preserved.
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
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin-macchiato",
    },
  },
}
