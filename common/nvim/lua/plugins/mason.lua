local platform = require("config.platform")

if platform.omarchy then
  -- Keep the Haskell debugger project-local without disabling the Haskell extra.
  return {
    {
      "mason-org/mason.nvim",
      opts = function(_, opts)
        opts.ensure_installed = vim.tbl_filter(function(tool)
          return tool ~= "haskell-debug-adapter"
        end, opts.ensure_installed or {})
      end,
    },
    {
      "jay-babu/mason-nvim-dap.nvim",
      optional = true,
      opts = function(_, opts)
        -- DAP uses the adapter name, not Mason's package name.
        opts.ensure_installed = vim.tbl_filter(function(adapter)
          return adapter ~= "haskell"
        end, opts.ensure_installed or {})
        if not opts.automatic_installation then
          return
        end
        local automatic = type(opts.automatic_installation) == "table" and opts.automatic_installation or {}
        automatic.exclude = automatic.exclude or {}
        if not vim.tbl_contains(automatic.exclude, "haskell") then
          table.insert(automatic.exclude, "haskell")
        end
        opts.automatic_installation = automatic
      end,
    },
  }
end

if not platform.nixos then
  return {}
end

-- Tools installed via Nix (home.packages or per-project flake) should NOT
-- also be installed by Mason. Strip them from any `ensure_installed` lists
-- that LazyVim extras inject.
local nix_managed = {
  delve = true,
  -- add others here if more tools migrate to Nix
}

return {
  {
    "mason-org/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = vim.tbl_filter(function(tool)
        return not nix_managed[tool]
      end, opts.ensure_installed or {})
    end,
  },
}
