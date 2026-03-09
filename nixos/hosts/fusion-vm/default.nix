{ config, pkgs, lib, inputs, ... }:
{
  imports = [
    ./disko.nix
    # ./hardware-configuration.nix  # uncomment after first nixos-anywhere run
  ];

  nixpkgs.overlays = [ inputs.claude-code.overlays.default ];

  networking.hostName = "fusion-vm";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Hyprland
  programs.hyprland = {
    enable = true;
    withUWSM = true;
  };
  programs.uwsm.enable = true;

  users.users.eduardo.shell = pkgs.nushell;
  users.users.eduardo.extraGroups = [ "input" "uinput" "kvm" ];

  virtualisation.libvirtd.enable = true;

  # Fix FHS path assumption in libvirt's upstream systemd unit
  # Empty string first clears the inherited ExecStart before setting the new one
  systemd.services.virt-secret-init-encryption.serviceConfig.ExecStart = lib.mkForce [
    ""
    "${pkgs.bash}/bin/sh -c 'umask 0077 && (dd if=/dev/random status=none bs=32 count=1 | systemd-creds encrypt --name=secrets-encryption-key - /var/lib/libvirt/secrets/secrets-encryption-key)'"
  ];

  hardware.uinput.enable = true;

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

  system.stateVersion = "24.11";
}
