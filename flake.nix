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
    nixpkgs-claude-code.url = "github:NixOS/nixpkgs/9fbc064e90a066853b73d4838564ac7ad49b6956";
  };

  outputs =
    {
      nixpkgs,
      nixpkgs-talos,
      nixpkgs-claude-code,
      home-manager,
      nix-darwin,
      ...
    }:
    let
      # ── Helpers to build pinned package sets for any system ─────────────
      mkPkgsTalos = system: import nixpkgs-talos { inherit system; };
      mkPkgsClaudeCode =
        system:
        import nixpkgs-claude-code {
          inherit system;
          config.allowUnfree = true;
        };

      # ── Shared extra args passed to every module ────────────────────────
      mkExtraArgs = system: username: {
        pkgs-talos = mkPkgsTalos system;
        pkgs-claude-code = mkPkgsClaudeCode system;
        dotfiles = ./.;
        inherit username;
      };
    in
    {
      # ── macOS (nix-darwin) — Work MacBook Pro ───────────────────────────
      darwinConfigurations."work-mbp" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = mkExtraArgs "aarch64-darwin" "paulgrillenberger";
        modules = [
          ./hosts/darwin-work.nix
          ./modules/darwin.nix
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = mkExtraArgs "aarch64-darwin" "paulgrillenberger";
              users.paulgrillenberger = {
                imports = [
                  ./home/common.nix
                  ./home/darwin.nix
                ];
              };
            };
          }
        ];
      };

      # ── macOS (nix-darwin) — Mac Mini ──────────────────────────────────
      darwinConfigurations."mini" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = mkExtraArgs "aarch64-darwin" "paul";
        modules = [
          ./hosts/darwin-mini.nix
          ./modules/darwin.nix
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = mkExtraArgs "aarch64-darwin" "paul";
              users.paul = {
                imports = [
                  ./home/common.nix
                  ./home/darwin.nix
                ];
              };
            };
          }
        ];
      };

      # ── NixOS ──────────────────────────────────────────────────────────
      nixosConfigurations."home-laptop" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = mkExtraArgs "x86_64-linux" "paulgrillenberger";
        modules = [
          ./hosts/nixos-home.nix
          ./modules/nixos.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = mkExtraArgs "x86_64-linux" "paulgrillenberger";
              users.paulgrillenberger = {
                imports = [
                  ./home/common.nix
                  ./home/nixos.nix
                ];
              };
            };
          }
        ];
      };

      # ── Standalone home-manager (fallback) ─────────────────────────────
      homeConfigurations."paulgrillenberger" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-darwin";
        extraSpecialArgs = mkExtraArgs "aarch64-darwin" "paulgrillenberger";
        modules = [
          { nixpkgs.config.allowUnfree = true; }
          ./home/common.nix
          ./home/darwin.nix
        ];
      };
    };
}
