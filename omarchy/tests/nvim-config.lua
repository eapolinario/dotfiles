local repo = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h:h")
local config = repo .. "/common/nvim/"
local function load_config(path)
  return dofile(config .. path)
end

local function equal(expected, actual)
  assert(vim.deep_equal(expected, actual), "expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual))
end

local original_has = vim.fn.has
local original_readable = vim.fn.filereadable
local original_directory = vim.fn.isdirectory
local inherited_keys = {
  { "<leader><space>", "files" },
  { "<leader>ff", "files" },
  { "<leader>sg", "grep" },
  { "<leader>sw", "word" },
  { "<leader>sw", "selection", mode = "x" },
  { "<leader>sG", "grep cwd" },
}

for _, name in ipairs({ "omarchy", "nixos", "macos", "linux" }) do
  vim.fn.has = function(feature)
    return feature == "mac" and name == "macos" and 1 or 0
  end
  vim.fn.filereadable = function(path)
    return path == "/etc/NIXOS" and name == "nixos" and 1 or 0
  end
  vim.fn.isdirectory = function()
    -- An Omarchy checkout must not override a known NixOS/macOS host.
    return name ~= "linux" and 1 or 0
  end
  local platform = load_config("lua/config/platform.lua")
  equal({
    omarchy = name == "omarchy",
    nixos = name == "nixos",
    macos = name == "macos",
  }, platform)
  package.loaded["config.platform"] = platform
  vim.fn.has = original_has
  vim.fn.filereadable = original_readable
  vim.fn.isdirectory = original_directory

  local lsp = load_config("lua/plugins/lsp.lua")
  equal(true, lsp[1].opts.codelens.enabled)
  if platform.nixos then
    equal(false, lsp[1].opts.servers.nixd.mason)
    equal(false, lsp[1].opts.servers.marksman.mason)
    equal({}, lsp[2].opts.ensure_installed)
  elseif platform.macos then
    equal(false, lsp[1].opts.servers.gopls.mason)
    equal(false, lsp[1].opts.servers.nixd.enabled)
  else
    equal(nil, lsp[1].opts.servers)
    equal(1, #lsp)
  end

  local mason = load_config("lua/plugins/mason.lua")
  if platform.nixos then
    local opts = { ensure_installed = { "delve", "stylua" } }
    mason[1].opts(nil, opts)
    equal({ "stylua" }, opts.ensure_installed)
  else
    equal({}, mason)
  end

  local colorscheme = load_config("lua/plugins/colorscheme.lua")
  if platform.omarchy then
    equal({}, colorscheme)
  else
    equal("nordic", colorscheme[3].opts.colorscheme)
  end
  equal(platform.omarchy, load_config("lua/plugins/fff.lua").enabled)

  local clipboard_calls = 0
  package.loaded["config.remote_clipboard"] = {
    setup = function()
      clipboard_calls = clipboard_calls + 1
    end,
  }
  vim.opt.relativenumber = true
  vim.g.autoformat = true
  vim.g.snacks_animate = true
  load_config("lua/config/options.lua")
  equal(platform.omarchy and 1 or 0, clipboard_calls)
  equal(not platform.omarchy, vim.opt.relativenumber:get())
  equal(not platform.omarchy, vim.g.autoformat)
  equal(platform.omarchy, vim.g.snacks_animate)

  local calls = {}
  package.loaded["telescope.builtin"] = {
    live_grep = function(opts)
      calls.grep = opts
    end,
    find_files = function(opts)
      calls.files = opts
    end,
  }
  package.loaded["fff"] = {
    live_grep = function()
      calls.fff = true
    end,
  }
  _G.LazyVim = {
    pick = function(picker)
      equal("live_grep", picker)
      return function()
        calls.telescope = true
      end
    end,
  }
  local telescope = load_config("lua/plugins/telescope.lua")
  local snacks = telescope[2]
  equal("folke/snacks.nvim", snacks[1])
  equal({ "<leader>.", false }, snacks.keys[1])
  equal("<leader>bs", snacks.keys[2][1])
  local scratch_calls = 0
  _G.Snacks = {
    scratch = function()
      scratch_calls = scratch_calls + 1
    end,
  }
  snacks.keys[2][2]()
  equal(1, scratch_calls)

  local keys = telescope[1].keys(nil, vim.deepcopy(inherited_keys))
  local mappings = {}
  for _, key in ipairs(keys) do
    mappings[key[1]] = key[2]
  end
  for _, lhs in ipairs({ "<leader><space>", "<leader>ff", "<leader>sg", "<leader>sw" }) do
    equal(not platform.omarchy, mappings[lhs] ~= nil)
  end
  equal("grep cwd", mappings["<leader>sG"])

  mappings["<leader>sp"]()
  equal(platform.omarchy, calls.fff == true)
  equal(not platform.omarchy, calls.telescope == true)
  vim.api.nvim_buf_set_name(0, "")
  mappings["<leader>sd"]()
  equal(vim.fn.getcwd(), calls.grep.cwd)
  vim.api.nvim_buf_set_name(0, config .. "init.lua")
  mappings["<leader>sd"]()
  equal(config:sub(1, -2), calls.grep.cwd)
  for _, lhs in ipairs({ "<leader>.", "<leader>fD" }) do
    mappings[lhs]()
    equal(config:sub(1, -2), calls.files.cwd)
  end
  mappings["<leader>sB"]()
  equal(true, calls.grep.grep_open_files)
end

local toggle_key
_G.Snacks = {
  toggle = function()
    return {
      map = function(_, key)
        toggle_key = key
      end,
    }
  end,
}
local blame = load_config("lua/plugins/gitsigns-blame.lua").opts(nil, { signcolumn = false })
equal("<leader>uB", toggle_key)
equal(true, blame.current_line_blame)
equal(300, blame.current_line_blame_opts.delay)
equal(false, blame.signcolumn)

local extras = vim.json.decode(table.concat(vim.fn.readfile(config .. "lazyvim.json"), "\n")).extras
equal(21, #extras)
local seen = {}
for _, extra in ipairs(extras) do
  assert(not seen[extra], "duplicate extra: " .. extra)
  seen[extra] = true
end
for _, extra in ipairs({ "editor.overseer", "editor.telescope", "dap.core", "lang.go", "lang.zig" }) do
  equal(true, seen["lazyvim.plugins.extras." .. extra])
end

print("Neovim configuration tests passed (Omarchy, NixOS, macOS, generic Linux)")
