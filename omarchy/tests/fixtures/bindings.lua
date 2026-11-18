local test_dir = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
local getenv = os.getenv
assert(getenv("SWAP_TEST_STATE"), "Run through omarchy/tests/workspace.sh with isolated fixtures")
local home = assert(getenv("HOME"))
local xdg = assert(getenv("XDG_CONFIG_HOME"))
local bindings
local close_action = { dispatcher = "window.close" }

hl = {
  unbind = function(key)
    bindings[key] = nil
  end,
  bind = function(key, action, options)
    bindings[key] = bindings[key] or {}
    table.insert(bindings[key], { action = action, description = (options or {}).description })
  end,
  dsp = { window = { close = function() return close_action end } },
}
-- Model helper registration without executing application or desktop actions.
o = {
  bind = function(key, description, action)
    hl.bind(key, action, { description = description })
  end,
  rebind = function(key, description, action)
    hl.unbind(key)
    o.bind(key, description, action)
  end,
}

local function only_binding(key, name)
  local entries = bindings[key] or {}
  assert(#entries == 1,
    ("%s: %s has %d registrations, expected 1"):format(name, key, #entries))
  return entries[1]
end

vim.o.shell = "/bin/sh"
for _, case in ipairs({
  { name = "XDG with spaces and apostrophes", xdg = xdg, config = xdg },
  { name = "HOME fallback", config = home .. "/.config" },
  { name = "empty XDG fallback", xdg = "", config = home .. "/.config" },
}) do
  os.getenv = function(name)
    if name == "XDG_CONFIG_HOME" then
      return case.xdg
    elseif name == "HOME" then
      return home
    end
    return getenv(name)
  end
  bindings = {}
  -- Relevant defaults from Omarchy 4.5.0-3; o.bind appends rather than replaces.
  o.bind("SUPER + Q", "Close window", hl.dsp.window.close())
  o.bind("SUPER + SHIFT + T", "Activity", { tui = "btop" })
  -- Also seed the legacy close shortcut to verify its intentional removal.
  o.bind("SUPER + W", "Close window", hl.dsp.window.close())

  dofile(test_dir .. "/../hypr/.config/hypr/bindings.lua")
  os.getenv = getenv
  local close = only_binding("SUPER + Q", case.name)
  assert(close.action == close_action, case.name .. ": wrong close action")
  local activity = only_binding("SUPER + SHIFT + T", case.name)
  assert(activity.description == "Activity" and vim.deep_equal(activity.action, { tui = "btop" }),
    case.name .. ": wrong Activity action")
  assert(#(bindings["SUPER + W"] or {}) == 0, case.name .. ": legacy close shortcut remains")

  local swap = only_binding("SUPER + SHIFT + S", case.name)
  assert(swap.description == "Swap workspace", case.name .. ": wrong workspace binding")
  local command = swap.action
  local script = case.config .. "/hypr/scripts/swap-workspace.sh"
  assert(command == vim.fn.shellescape(script), case.name .. ": wrong script path")
  assert(vim.fn.resolve(script) == test_dir .. "/fixtures/workspace-command.sh",
    case.name .. ": refusing to launch a non-fixture script")
  local output = vim.fn.system(command)
  assert(vim.v.shell_error == 0, case.name .. ": command failed: " .. output)
  assert(output == "workspace-binding-ok\n", case.name .. ": script did not launch")
  print("ok - " .. case.name)
end
