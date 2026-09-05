local macos = vim.fn.has("mac") == 1
-- Nix on PATH does not mean the host's language servers are managed by NixOS.
local nixos = not macos and vim.fn.filereadable("/etc/NIXOS") == 1
local omarchy_path = vim.env.OMARCHY_PATH or vim.fn.expand("~/.local/share/omarchy")

return {
  macos = macos,
  nixos = nixos,
  omarchy = not macos and not nixos and vim.fn.isdirectory(omarchy_path) == 1,
}
