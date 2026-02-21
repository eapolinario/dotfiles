{ config, pkgs, lib, ... }:
{
  imports = [
    ./disko.nix
    # ./hardware-configuration.nix  # uncomment after first nixos-anywhere run
  ];

  networking.hostName = "example"; # rename to match mkHost key in flake.nix

  # Host-specific packages and services go here

  system.stateVersion = "24.11";
}
