{
  config,
  pkgs,
  lib,
  ...
}:
{
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "symbola"
      "claude-code"
      "copilot-language-server"
      "opencode"
      "vscode"
    ];

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = true;
    # nix-community binary cache. Serves prebuilt emacs-overlay artifacts
    # (emacs-unstable-pgtk, etc.) so they download instead of building from
    # source. extra-* appends to the default cache.nixos.org substituter.
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  environment.systemPackages = with pkgs; [
    git
    curl
    wget
    cacert
    clang
    cmake
    gnumake
    libtool
  ];

  # Expose the system CA bundle to graphical sessions (Emacs, etc.).
  # Without this, tools launched outside an interactive shell (e.g. curl via
  # plz/org-web-tools from a GUI Emacs) fail TLS verification with curl error 60.
  environment.sessionVariables = {
    SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
  };

  programs.nix-ld.enable = true;

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

  time.timeZone = "America/New_York";

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
