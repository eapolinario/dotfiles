-- FFF keeps Omarchy's primary searches; Telescope provides the other pickers
-- and the shared Doom-style "search from here" bindings.
return {
  "dmtrKovalenko/fff",
  enabled = require("config.platform").omarchy,
  -- Fetches a prebuilt binary, falling back to `cargo build --release`.
  build = function()
    require("fff.download").download_or_build_binary()
  end,
  -- Start indexing before the first search.
  lazy = false,
  opts = {
    layout = { prompt_position = "top" },
  },
  keys = {
    -- stylua: ignore start
    { "<leader><space>", function() require("fff").find_files() end, desc = "Find Files (fff)" },
    { "<leader>ff", function() require("fff").find_files() end, desc = "Find Files (fff)" },
    { "<leader>sg", function() require("fff").live_grep() end, desc = "Grep (fff)" },
    { "<leader>sw", function() require("fff").live_grep_under_cursor() end, mode = { "n", "x" }, desc = "Grep Word (fff)" },
    -- stylua: ignore end
  },
}
