-- Go-specific DAP keymaps. `<leader>dt` is taken by LazyVim's DAP core
-- (Terminate), so debug-test lives under `<leader>dT` (mnemonic: Debug Test).
-- Extract a `-tags=...` flag from $GOFLAGS, if any. Lets per-project
-- direnv (`export GOFLAGS=-tags=e2e`) propagate into dlv's `buildFlags`.
-- Needed because nvim-dap-go's `debug_test` sets `buildFlags` explicitly,
-- which would otherwise stomp the GOFLAGS the `go` toolchain honors.
local function build_flags_from_env()
  local gf = vim.env.GOFLAGS or ""
  return gf:match("%-tags=%S+") or ""
end

return {
  {
    "leoluz/nvim-dap-go",
    optional = true,
    opts = function()
      return {
        delve = {
          build_flags = build_flags_from_env(),
        },
      }
    end,
    keys = {
      {
        "<leader>dT",
        function() require("dap-go").debug_test() end,
        desc = "Debug Nearest Go Test",
        ft = "go",
      },
      {
        "<leader>dL",
        function() require("dap-go").debug_last_test() end,
        desc = "Debug Last Go Test",
        ft = "go",
      },
    },
  },
}
