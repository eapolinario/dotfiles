{
  description = "Eduardo's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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
  };

  outputs = { self, nixpkgs, disko, home-manager, llm-agents, ... }@inputs:
    let
      lib = nixpkgs.lib;
      system = "aarch64-linux";
      host_names = [ "fusion-vm" ];

      mkHost = hostname: lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit inputs; };
            home-manager.users.eduardo = {
              imports = [ inputs.xremap-flake.homeManagerModules.default ./home/eduardo ];
            };
          }
          ({ pkgs, ... }: {
            nixpkgs.overlays = [
              llm-agents.overlays.default
            ];
            environment.systemPackages = [
              pkgs.llm-agents.claude-code-acp
              pkgs.llm-agents.copilot-cli
              pkgs.llm-agents.agentsview
              pkgs.llm-agents.rtk
              pkgs.llm-agents.pi
              # pkgs.llm-agents.codex-acp
            ];
          })
          ./hosts/${hostname}
          ./modules/common
        ];
      };

      nixos_configurations = lib.genAttrs host_names mkHost;
      nixos_checks = lib.mapAttrs'
        (hostname: config:
          lib.nameValuePair "nixos-${hostname}-toplevel" config.config.system.build.toplevel)
        nixos_configurations;
    in
    {
      nixosConfigurations = nixos_configurations;
      checks.${system} = nixos_checks;
    };
}
