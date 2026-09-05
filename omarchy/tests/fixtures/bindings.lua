local test_dir = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
local getenv = os.getenv
assert(getenv("SWAP_TEST_STATE"), "Run through omarchy/tests/workspace.sh with isolated fixtures")
local home = assert(getenv("HOME"))
local xdg = assert(getenv("XDG_CONFIG_HOME"))
local command

hl = {
  unbind = function() end,
  bind = function() end,
  dsp = { window = { close = function() return {} end } },
}
o = {
  bind = function(key, description, action)
    if description == "Swap workspace" then
      assert(key == "SUPER + SHIFT + S")
      command = action
    end
  end,
}

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
  command = nil
  dofile(test_dir .. "/../hypr/.config/hypr/bindings.lua")
  os.getenv = getenv
  local script = case.config .. "/hypr/scripts/swap-workspace.sh"
  assert(command == vim.fn.shellescape(script), case.name .. ": wrong script path")
  assert(vim.fn.resolve(script) == test_dir .. "/fixtures/workspace-command.sh",
    case.name .. ": refusing to launch a non-fixture script")
  local output = vim.fn.system(command)
  assert(vim.v.shell_error == 0, case.name .. ": command failed: " .. output)
  assert(output == "workspace-binding-ok\n", case.name .. ": script did not launch")
  print("ok - " .. case.name)
end
