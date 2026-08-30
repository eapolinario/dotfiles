{
  description = "Eduardo's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixos-anywhere = {
      url = "github:nix-community/nixos-anywhere";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    claude-code.url = "github:sadjow/claude-code-nix";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xremap-flake = {
      url = "github:xremap/nix-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents.url = "github:numtide/llm-agents.nix";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      disko,
      home-manager,
      llm-agents,
      ...
    }@inputs:
    let
      lib = nixpkgs.lib;

      hosts = {
        fusion-vm = {
          system = "aarch64-linux";
          modules = [ ./hosts/fusion-vm ];
          ci = {
            eval = true;
            build = true;
            runner = "ubuntu-24.04-arm";
          };
        };
      };

      mkHost =
        hostname: host:
        lib.nixosSystem {
          inherit (host) system;
          specialArgs = { inherit inputs; };
          modules = [
            disko.nixosModules.disko
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };
              home-manager.users.eduardo = {
                imports = [
                  inputs.xremap-flake.homeManagerModules.default
                  ./home/eduardo
                ];
              };
            }
            (
              { pkgs, ... }:
              {
                nixpkgs.overlays = [
                  llm-agents.overlays.shared-nixpkgs
                  inputs.emacs-overlay.overlays.default
                ];
                environment.systemPackages = with pkgs.llm-agents; [
                  copilot-cli
                  pi
                  rtk
                  sidecar
                  # pi-acp might come in https://github.com/svkozak/pi-acp
                  # pi-acp
                  # codex-acp
                ];
              }
            )
          ]
          ++ host.modules
          ++ [
            ./modules/common
          ];
        };

      nixos_configurations = lib.mapAttrs mkHost hosts;
      nixos_checks = lib.foldlAttrs (
        acc: hostname: host:
        acc
        // {
          ${host.system} = (acc.${host.system} or { }) // {
            "nixos-${hostname}-toplevel" = nixos_configurations.${hostname}.config.system.build.toplevel;
          };
        }
      ) { } hosts;

      host_metadata = lib.mapAttrs (_: host: {
        inherit (host) system ci;
      }) hosts;

      ci_eval_hosts = builtins.attrNames (lib.filterAttrs (_: host: host.ci.eval or false) hosts);
      ci_build_hosts = lib.filterAttrs (_: host: host.ci.build or false) hosts;
      ci_build_host_names = builtins.attrNames ci_build_hosts;
      default_build_host = if ci_build_host_names == [ ] then null else builtins.head ci_build_host_names;

      host_systems = lib.unique (lib.mapAttrsToList (_: host: host.system) hosts);

      formatter_systems = lib.unique (
        host_systems
        ++ [
          "x86_64-linux"
          "aarch64-linux"
        ]
      );
    in
    {
      nixosConfigurations = nixos_configurations;
      checks = nixos_checks;
      formatter = lib.genAttrs formatter_systems (system: nixpkgs.legacyPackages.${system}.nixfmt-tree);
      lib = {
        hostMetadata = host_metadata;
        ciMetadata = {
          evalHosts = ci_eval_hosts;
          evalHostsText = lib.concatStringsSep " " ci_eval_hosts;
          buildHosts = lib.mapAttrs (_: host: {
            inherit (host) system;
            runner = host.ci.runner;
          }) ci_build_hosts;
          defaultBuildHost =
            if default_build_host == null then
              null
            else
              {
                name = default_build_host;
                inherit (ci_build_hosts.${default_build_host}) system;
                runner = ci_build_hosts.${default_build_host}.ci.runner;
              };
        };
      };
    };
}
