if not require("config.platform").nixos then
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
