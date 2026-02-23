{ config, pkgs, lib, inputs, ... }:
{
  home.username = "eduardo";
  home.homeDirectory = "/home/eduardo";
  home.stateVersion = "24.11";

  home.packages = with pkgs; [
    # Desktop apps
    ghostty
    foot
    wofi
    waybar
    chromium
    bitwarden-desktop
    bitwarden-cli
    wlr-randr
    emacs30-pgtk

    # Dev tools
    claude-code
    gnupg
    clang
    cmake
    gnumake
    nodejs_24
    neovim
    ripgrep
    direnv
    git-crypt
    fd
  ];

  home.file.".config/hypr/hyprland.conf".source = ../../hypr/hyprland.conf;

  programs.nushell.enable = true;

  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.home-manager.enable = true;
}
