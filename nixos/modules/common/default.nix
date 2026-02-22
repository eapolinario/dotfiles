{ config, pkgs, lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "symbola"
    "claude-code"
  ];

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true;
  };

  environment.systemPackages = with pkgs; [
    git
    neovim
    curl
    wget
    ripgrep
    direnv
    fd
    cmake
    gnumake
    nodejs_24
  ];

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  users.users.eduardo = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDrpGHLZ1qcfDZ44mRT68Dluhx08d54pC0Nek1IXAkve eapolinario@users.noreply.github.com"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      nerd-fonts.fira-code
      symbola
    ];
    fontconfig.defaultFonts.monospace = [ "FiraCode Nerd Font Mono" ];
  };
}
