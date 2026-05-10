{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
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
    imv
    maim
    seahorse
    vscode
    wl-clipboard
    wlr-randr
    elephant
    walker
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
    jq
    just
    lazygit
    libsecret
    neovim
    nixd
    nixfmt
    nodejs_24
    opencode
    pandoc
    (ripgrep.override { withPCRE2 = true; })
    sqlite
    squashfsTools
    tree
    unzip
    uv
    piShell
  ];

  home.file.".config/hypr/hyprland.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/nixos/hypr/hyprland.conf";

  home.file.".config/doom".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/doom";

  home.file.".authinfo".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/authinfo/.authinfo";

  # Claude Code config: enables the caveman plugin/marketplace and points the
  # statusline at the wrapper hook. Symlink individual files so Claude Code can
  # still write state (plugins/, projects/, .caveman-active, ...) into ~/.claude.
  # The caveman skill itself is fetched at runtime by Claude Code from the
  # marketplace declared in settings.json (no nix-side fetch needed).
  # force = true: the apps write these files themselves on first launch, so
  # they pre-exist as plain (non-symlink) files. Without force, home-manager
  # activation aborts with an "existing file in the way" error. With force, it
  # replaces them with the managed symlink. Tradeoff: the apps can no longer
  # mutate these specific files (they live in the read-only nix store) — any
  # change must be made in dotfiles/common/.
  home.file.".claude/settings.json" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/claude/.claude/settings.json";
    force = true;
  };

  home.file.".claude/hooks/caveman-statusline.sh" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/claude/.claude/hooks/caveman-statusline.sh";
    force = true;
  };

  # pi agent: settings.json adds the caveman skill paths and the caveman-status
  # extension mirrors the plugin state in pi's footer. Symlink individual files
  # so pi can keep auth.json / sessions/ alongside under ~/.pi/agent.
  home.file.".pi/agent/settings.json" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/pi/.pi/agent/settings.json";
    force = true;
  };

  home.file.".pi/agent/extensions/caveman-status.ts" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/pi/.pi/agent/extensions/caveman-status.ts";
    force = true;
  };

  home.file.".pi/agent/prompts/caveman.md" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/common/pi/.pi/agent/prompts/caveman.md";
    force = true;
  };

  home.file."org".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repos/org-files/source_files";

  xdg.configFile."ghostty/config".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/nixos/home/eduardo/ghostty/config";

  # Experimental pi shell integration for Nushell.
  xdg.configFile."nushell/pi-agent.nu".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/nixos/home/eduardo/nushell/pi-agent.nu";

  programs.waybar = {
    enable = true;
    settings = [
      {
        layer = "top";
        position = "top";
        height = 32;
        modules-left = [
          "hyprland/workspaces"
          "hyprland/window"
        ];
        modules-center = [ "clock" ];
        modules-right = [
          "cpu"
          "memory"
          "network"
          "tray"
        ];

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
      }
    ];

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

  programs.carapace = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableNushellIntegration = true;
    # Gruvbox Rainbow preset from https://starship.rs/presets/gruvbox-rainbow
    # Imported verbatim so it stays in sync with upstream.
    settings = builtins.fromTOML (builtins.readFile ./starship/gruvbox-rainbow.toml);
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
            "C-k" = {
              launch = [
                "wtype"
                "-M"
                "shift"
                "-k"
                "End"
                "-m"
                "shift"
                "-k"
                "BackSpace"
              ];
            };
            "Super-n" = "C-n";
          };
        }
      ];
    };
  };
}
