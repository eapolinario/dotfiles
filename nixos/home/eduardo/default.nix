{ config, pkgs, lib, inputs, ... }:
let
  piShell = pkgs.writeShellScriptBin "pi-shell" (builtins.readFile ./nushell/pi-shell.sh);
in
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
    # emacs is configured via programs.emacs below
    foot
    ghostty
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
    # direnv is configured via programs.direnv below
    fd
    firecracker
    gh
    git-crypt
    gnupg
    go_1_25
    gopls
    graphviz
    just
    libsecret
    neovim
    nixd
    nodejs_24
    opencode
    (ripgrep.override { withPCRE2 = true; })
    sqlite
    squashfsTools
    tree
    uv
    piShell
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

  xdg.configFile."wofi/config".text = ''
    show=drun
    prompt=Run
    width=720
    height=480
    lines=10
    columns=1
    location=center
    allow_images=true
    allow_markup=true
    image_size=32
    insensitive=true
    no_actions=true
    hide_scroll=true
    term=ghostty
    matching=fuzzy
    gtk_dark=true
    key_expand=Tab
    key_exit=Escape
  '';

  xdg.configFile."wofi/style.css".text = ''
    * {
      font-family: "FiraCode Nerd Font Mono", monospace;
      font-size: 13px;
    }

    window {
      background-color: rgba(30, 30, 46, 0.96);
      border: 2px solid #cba6f7;
      border-radius: 12px;
      color: #cdd6f4;
    }

    #outer-box {
      margin: 12px;
      padding: 8px;
    }

    #input {
      margin: 4px 4px 12px 4px;
      padding: 8px 12px;
      border: none;
      border-radius: 8px;
      background-color: #313244;
      color: #cdd6f4;
      caret-color: #cba6f7;
    }

    #input:focus {
      outline: none;
      box-shadow: 0 0 0 2px #cba6f7;
    }

    #input image {
      color: #cba6f7;
    }

    #scroll {
      margin: 0 4px;
    }

    #inner-box {
      background-color: transparent;
    }

    #img {
      margin-right: 10px;
      -gtk-icon-transform: scale(1.0);
    }

    #text {
      color: #cdd6f4;
    }

    #entry {
      padding: 8px 12px;
      margin: 2px 0;
      border-radius: 8px;
      background-color: transparent;
    }

    #entry:hover {
      background-color: #313244;
    }

    #entry:selected,
    #entry:selected #text {
      background-color: #45475a;
      color: #f5e0dc;
    }

    #entry:selected {
      box-shadow: inset 3px 0 0 0 #cba6f7;
    }

    #unselected {
      color: #cdd6f4;
    }
  '';

  # Experimental pi shell integration for Nushell.
  xdg.configFile."nushell/pi-agent.nu".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/nixos/home/eduardo/nushell/pi-agent.nu";

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

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable-pgtk;
  };

  programs.nushell = {
    enable = true;
    extraConfig = ''
      source ~/.config/nushell/pi-agent.nu

      $env.EDITOR = "emacs"

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

  programs.direnv = {
    enable = true;
    enableNushellIntegration = true;
    nix-direnv.enable = true;
  };

  programs.starship = {
    enable = true;
    enableNushellIntegration = true;
    settings = {
      add_newline = false;
      format = "$directory$git_branch$git_status\n$character";

      character = {
        error_symbol = "[x](bold red)";
        success_symbol = "[>](bold cyan)";
      };

      directory = {
        fish_style_pwd_dir_length = 1;
        truncation_length = 3;
        truncation_symbol = "../";
      };

      git_branch = {
        format = "on [$branch]($style) ";
        style = "bold cyan";
      };

      git_status = {
        format = "([$all_status$ahead_behind]($style) )";
        style = "cyan";
      };
    };
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
