{ config, pkgs, lib, ... }:
{
  imports = [
    ./disko.nix
    # ./hardware-configuration.nix  # uncomment after first nixos-anywhere run
  ];

  networking.hostName = "fusion-vm";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Hyprland
  programs.hyprland = {
    enable = true;
    withUWSM = true;
  };
  programs.uwsm.enable = true;

  # Link hyprland config into eduardo's home via tmpfiles
  systemd.tmpfiles.rules = [
    "L+ /home/eduardo/.config/hypr/hyprland.conf - - - - ${../../hypr/hyprland.conf}"
    "L+ /home/eduardo/.config/doom - - - - /home/eduardo/dotfiles/doom"
  ];

  environment.systemPackages = with pkgs; [
    ghostty # terminal
    foot
    wofi   # app launcher
    waybar   # status bar
    chromium
    wlr-randr
    emacs30-pgtk
  ];

  # Display manager
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd 'uwsm start hyprland-uwsm.desktop'";
        user = "eduardo";
      };
      initial_session = {
        command = "uwsm start hyprland-uwsm.desktop";
        user = "eduardo";
      };
    };
  };

  # VMware Fusion on aarch64 has no SVGA3D Mesa driver — force software rendering
  environment.variables.LIBGL_ALWAYS_SOFTWARE = "1";

  # Required for Wayland/Hyprland
  hardware.graphics.enable = true;

  # VMware guest tools (better graphics, clipboard, drag-and-drop)
  virtualisation.vmware.guest.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-hyprland ];
  };

  # Install Doom Emacs on first boot if not already present
  systemd.services.doom-emacs-install = {
    description = "Install Doom Emacs for eduardo";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "eduardo";
      ExecStart = pkgs.writeShellScript "doom-install" ''
        set -euo pipefail
        export HOME="/home/eduardo"
        export PATH="${lib.makeBinPath (with pkgs; [ bash coreutils findutils git emacs30-pgtk gnugrep gnused gawk ])}:/run/current-system/sw/bin"
        if [ ! -f "$HOME/.config/emacs/bin/doom" ]; then
          git clone --depth=1 https://github.com/doomemacs/doomemacs "$HOME/.config/emacs"
          "$HOME/.config/emacs/bin/doom" install --no-config
        fi
      '';
    };
  };

  system.stateVersion = "24.11";
}
