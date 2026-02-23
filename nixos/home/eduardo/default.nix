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
    chromium
    bitwarden-desktop
    bitwarden-cli
    wlr-randr
    wl-clipboard
    emacs30-pgtk

    # Dev tools
    claude-code
    gnupg
    clang
    cmake
    gnumake
    nodejs_24
    btop
    neovim
    ripgrep
    direnv
    git-crypt
    fd
  ];

  home.file.".config/hypr/hyprland.conf".source = ../../hypr/hyprland.conf;

  xdg.configFile."ghostty/config".text = ''
    command = nu
  '';

  programs.waybar = {
    enable = true;
    settings = [{
      layer = "top";
      position = "top";
      height = 32;
      modules-left = [ "hyprland/workspaces" "hyprland/window" ];
      modules-center = [ "clock" ];
      modules-right = [ "cpu" "memory" "network" "tray" ];

      "hyprland/workspaces" = {
        format = "{id}";
        on-click = "activate";
      };

      "hyprland/window" = {
        max-length = 60;
      };

      clock = {
        format = "{:%a %b %d  %H:%M}";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      };

      cpu = {
        format = "CPU {usage}%";
        interval = 2;
      };

      memory = {
        format = "MEM {}%";
        interval = 2;
      };

      network = {
        format-ethernet = "NET {ipaddr}";
        format-wifi = "WIFI {signalStrength}%";
        format-disconnected = "NET --";
        tooltip-format = "{ifname}: {ipaddr}";
      };

      tray = {
        spacing = 8;
      };
    }];

    style = ''
      * {
        font-family: "FiraCode Nerd Font Mono";
        font-size: 13px;
        min-height: 0;
      }

      window#waybar {
        background-color: #1e1e2e;
        color: #cdd6f4;
        border-bottom: 2px solid #cba6f7;
      }

      #workspaces button {
        padding: 0 8px;
        color: #585b70;
        background: transparent;
        border: none;
        border-radius: 0;
        box-shadow: none;
      }

      #workspaces button.active {
        color: #cba6f7;
      }

      #workspaces button:hover {
        background: #313244;
        box-shadow: none;
      }

      #window {
        padding: 0 10px;
        color: #cdd6f4;
      }

      #clock {
        padding: 0 10px;
        color: #b4befe;
      }

      #cpu {
        padding: 0 10px;
        color: #a6e3a1;
      }

      #memory {
        padding: 0 10px;
        color: #f9e2af;
      }

      #network {
        padding: 0 10px;
        color: #89b4fa;
      }

      #tray {
        padding: 0 8px;
      }
    '';
  };

  programs.nushell.enable = true;

  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.home-manager.enable = true;
}
