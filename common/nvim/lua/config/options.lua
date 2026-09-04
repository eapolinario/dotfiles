-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

if require("config.platform").omarchy then
  -- Keep Omarchy's clipboard integration and editor defaults.
  require("config.remote_clipboard").setup()
  vim.opt.relativenumber = false
  vim.g.autoformat = false
else
  -- Omarchy disables scrolling separately; elsewhere disable all Snacks animations.
  vim.g.snacks_animate = false
end
