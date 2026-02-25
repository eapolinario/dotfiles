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

    beads.url = "github:steveyegge/beads";
  };

  outputs = { self, nixpkgs, disko, home-manager, llm-agents, ... }@inputs:
    let
      mkHost = hostname: nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
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
          ({ pkgs, inputs, ... }: {
            nixpkgs.overlays = [ llm-agents.overlays.default ];
            environment.systemPackages = [
              inputs.beads.packages.${pkgs.stdenv.hostPlatform.system}.default
              pkgs.llm-agents.claude-code-acp
              pkgs.llm-agents.codex-acp
            ];
          })
          ./hosts/${hostname}
          ./modules/common
        ];
      };
    in
    {
      nixosConfigurations = {
        fusion-vm = mkHost "fusion-vm";
      };
    };
}
