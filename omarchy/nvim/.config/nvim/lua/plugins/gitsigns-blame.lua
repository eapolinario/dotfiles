-- LazyVim ships gitsigns with on-demand blame only (<leader>ghb / <leader>ghB).
-- Turn on the inline end-of-line annotation for the current line as well.
return {
  "lewis6991/gitsigns.nvim",
  opts = function(_, opts)
    Snacks.toggle({
      name = "Git Blame (inline)",
      get = function()
        return require("gitsigns.config").config.current_line_blame
      end,
      set = function(state)
        require("gitsigns").toggle_current_line_blame(state)
      end,
    }):map("<leader>uB")

    return vim.tbl_deep_extend("force", opts, {
      current_line_blame = true,
      current_line_blame_opts = {
        -- gitsigns defaults to 1000ms, which feels laggy while navigating.
        delay = 300,
        -- Attribute the last real change, not a reindent.
        ignore_whitespace = true,
      },
    })
  end,
}
