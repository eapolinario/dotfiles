-- fff: Rust-backed file/content finder with frecency ranking and a background
-- watcher. LazyVim ships no `fff` extra, so the whole spec lives here.
--
-- It takes over the picker keys worth the muscle memory. The snacks
-- equivalents stay reachable on their cwd variants (<leader>fF, <leader>sG,
-- <leader>sW) and every other <leader>f / <leader>s picker is untouched.
--
-- Upstream renamed the repo from `fff.nvim` to `fff`; run `:Lazy clean` if a
-- stale `fff.nvim` directory is left behind.
return {
  "dmtrKovalenko/fff",
  -- Fetches a prebuilt binary, falling back to `cargo build --release`.
  build = function()
    require("fff.download").download_or_build_binary()
  end,
  -- fff defers indexing itself and keeps a file watcher running, so loading it
  -- lazily off `keys` would only push the initial scan to the first search.
  lazy = false,
  opts = {
    -- fff anchors the prompt at the bottom; snacks puts it on top.
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
