-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

vim.api.nvim_create_autocmd("FileType", {
  pattern = "sidekick_terminal",
  callback = function()
    -- Preserve multiline input in Sidekick's terminal buffer without disabling the
    -- global Shift+Enter terminal shortcut elsewhere.
    vim.keymap.set({ "n", "i", "t" }, "<S-CR>", "<Nop>", {
      buffer = true,
      silent = true,
      desc = "Disable terminal shortcut inside Sidekick",
    })
  end,
})
