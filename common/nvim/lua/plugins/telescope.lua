-- Doom-Emacs-style "search from current file's directory" Telescope bindings.
-- These mirror `SPC s d` / `SPC .` from Doom.
--
-- Why this lives in a plugin spec instead of `config/keymaps.lua`:
-- LazyVim binds `<leader>sd` (Search Diagnostics) via the telescope plugin
-- spec's `keys = {...}` table. Lazy.nvim re-applies those when the plugin
-- loads, which clobbers anything we set with `vim.keymap.set` in
-- `config/keymaps.lua`. Overriding the same lhs *inside* the plugin spec
-- is the LazyVim-sanctioned way to win that race.
local function buf_dir()
  local path = vim.api.nvim_buf_get_name(0)
  if path == "" then
    return vim.fn.getcwd()
  end
  return vim.fn.fnamemodify(path, ":p:h")
end

return {
  {
    "nvim-telescope/telescope.nvim",
    keys = {
      {
        "<leader>sd",
        function()
          local dir = buf_dir()
          require("telescope.builtin").live_grep({
            cwd = dir,
            prompt_title = "Grep in " .. vim.fn.fnamemodify(dir, ":~"),
          })
        end,
        desc = "Search cwd of file",
      },
      {
        "<leader>fD",
        function()
          local dir = buf_dir()
          require("telescope.builtin").find_files({
            cwd = dir,
            prompt_title = "Find in " .. vim.fn.fnamemodify(dir, ":~"),
          })
        end,
        desc = "Find files in cwd of file",
      },
      -- Doom `SPC .` — find files in current buffer's directory.
      {
        "<leader>.",
        function()
          local dir = buf_dir()
          require("telescope.builtin").find_files({
            cwd = dir,
            prompt_title = "Find in " .. vim.fn.fnamemodify(dir, ":~"),
          })
        end,
        desc = "Find files in cwd of file",
      },
      -- Doom `SPC s B` — live grep across currently open buffers.
      {
        "<leader>sB",
        function()
          require("telescope.builtin").live_grep({
            grep_open_files = true,
            prompt_title = "Grep Open Buffers",
          })
        end,
        desc = "Grep open buffers",
      },
      -- Doom `SPC s p` — alias for LazyVim's `<leader>sg` (grep project root).
      {
        "<leader>sp",
        function()
          LazyVim.pick("live_grep")()
        end,
        desc = "Search project (alias of sg)",
      },
    },
  },
}
