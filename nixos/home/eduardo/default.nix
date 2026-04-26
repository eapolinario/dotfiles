{ config, pkgs, lib, inputs, ... }:
{
  home.username = "eduardo";
  home.homeDirectory = "/home/eduardo";
  home.stateVersion = "24.11";

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  home.packages = with pkgs; [
    # Desktop apps
    adwaita-icon-theme
    bitwarden-cli
    bitwarden-desktop
    chromium
    emacs30-pgtk
    foot
    ghostty
    gnome-keyring
    maim
    seahorse
    vscode
    wl-clipboard
    wlr-randr
    wofi
    wtype

    # Dev tools
    bat
    btop
    claude-code
    copilot-language-server
    direnv
    fd
    firecracker
    gh
    git-crypt
    gnupg
    go_1_25
    gopls
    graphviz
    just
    neovim
    nixd
    nodejs_24
    opencode
    (ripgrep.override { withPCRE2 = true; })
    sqlite
    squashfsTools
    tree
    uv
  ];

  home.file.".config/hypr/hyprland.conf".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/nixos/hypr/hyprland.conf";

  home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/common/doom";

  home.file.".authinfo".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/common/authinfo/.authinfo";

  home.file."org".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/repos/org-files/source_files";

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

  programs.nushell = {
    enable = true;
    extraConfig = ''
      $env.config = ($env.config | upsert keybindings (
        ($env.config.keybindings? | default []) ++ [
          {
            name: fzf_history
            modifier: control
            keycode: char_r
            mode: [emacs, vi_normal, vi_insert]
            event: {
              send: ExecuteHostCommand
              cmd: "commandline edit --replace (history | get command | uniq | reverse | str join (char newline) | fzf --layout=reverse --height 40% | str trim)"
            }
          }
          {
            name: fzf_files
            modifier: control
            keycode: char_t
            mode: [emacs, vi_normal, vi_insert]
            event: {
              send: ExecuteHostCommand
              cmd: "commandline edit --insert (fzf --layout=reverse --height 40% | str trim)"
            }
          }
        ]
      ))
    '';
  };

  services.gnome-keyring.enable = true;

  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-gnome3;
  };

  programs.fzf.enable = true;

  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.home-manager.enable = true;

  services.cliphist.enable = true;

  services.xremap = {
    enable = true;
    withWlroots = true;
    config = {
      keymap = [
        {
          name = "Chromium Emacs bindings";
          application.only = [ "chromium-browser" ];
          remap = {
            "C-a" = "Home";
            "C-e" = "End";
            "C-n" = "Down";
            "C-p" = "Up";
            "M-f" = "C-Right";
            "M-b" = "C-Left";
            "M-d" = "C-Delete";
            "M-BackSpace" = "C-BackSpace";
            "C-k" = { launch = ["wtype" "-M" "shift" "-k" "End" "-m" "shift" "-k" "BackSpace"]; };
            "Super-n" = "C-n";
          };
        }
      ];
    };
  };
}
