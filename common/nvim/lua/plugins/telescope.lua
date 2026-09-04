-- Doom-Emacs-style "search from current file's directory" Telescope bindings.
-- These mirror `SPC s d` / `SPC .` from Doom.
--
-- Why this lives in a plugin spec instead of `config/keymaps.lua`:
-- LazyVim binds `<leader>sd` (Search Diagnostics) via the telescope plugin
-- spec's `keys = {...}` table. Lazy.nvim re-applies those when the plugin
-- loads, which clobbers anything we set with `vim.keymap.set` in
-- `config/keymaps.lua`. Overriding the same lhs *inside* the plugin spec
-- is the LazyVim-sanctioned way to win that race.
local omarchy = require("config.platform").omarchy

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
    keys = function(_, keys)
      if omarchy then
        -- FFF owns these bindings, including both modes of <leader>sw.
        local fff_keys = {
          ["<leader><space>"] = true,
          ["<leader>ff"] = true,
          ["<leader>sg"] = true,
          ["<leader>sw"] = true,
        }
        keys = vim.tbl_filter(function(key)
          local lhs = type(key) == "string" and key or key[1]
          return not fff_keys[lhs]
        end, keys)
      end

      return vim.list_extend(keys, {
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
            if omarchy then
              require("fff").live_grep()
            else
              LazyVim.pick("live_grep")()
            end
          end,
          desc = "Search project (alias of sg)",
        },
      })
    end,
  },
}
