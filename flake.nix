{
  description = "Nix configuration for paulgrillenberger — macOS (nix-darwin) & NixOS";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-talos.url = "github:NixOS/nixpkgs/6a43094a5d05d6f0e2232d340baa9bae555ef232";
  };

  outputs = { nixpkgs, nixpkgs-talos, home-manager, nix-darwin, ... }:
    let
      # ── Helper to build pkgs-talos for any system ──────────────────────
      mkPkgsTalos = system: import nixpkgs-talos { inherit system; };

      # ── Shared extra args passed to every home-manager module ──────────
      mkExtraArgs = system: {
        pkgs-talos = mkPkgsTalos system;
        dotfiles = ./.;
      };
    in
    {
      # ── macOS (nix-darwin) ─────────────────────────────────────────────
      darwinConfigurations."work-mbp" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = mkExtraArgs "aarch64-darwin";
        modules = [
          ./hosts/darwin-work.nix
          ./modules/darwin.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = mkExtraArgs "aarch64-darwin";
            home-manager.users.paulgrillenberger = {
              imports = [ ./home/common.nix ./home/darwin.nix ];
            };
          }
        ];
      };

      # ── NixOS ──────────────────────────────────────────────────────────
      nixosConfigurations."home-laptop" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = mkExtraArgs "x86_64-linux";
        modules = [
          ./hosts/nixos-home.nix
          ./modules/nixos.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = mkExtraArgs "x86_64-linux";
            home-manager.users.paulgrillenberger = {
              imports = [ ./home/common.nix ./home/nixos.nix ];
            };
          }
        ];
      };

      # ── Standalone home-manager (fallback) ─────────────────────────────
      homeConfigurations."paulgrillenberger" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-darwin";
        extraSpecialArgs = mkExtraArgs "aarch64-darwin";
        modules = [
          { nixpkgs.config.allowUnfree = true; }
          ./home/common.nix
          ./home/darwin.nix
        ];
      };
    };
}
