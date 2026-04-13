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
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nix-darwin,
      ...
    }:
    let
      # ── Overlay to pin specific package versions ────────────────────────
      overlay = import ./overlays;

      # ── Shared extra args passed to every module ────────────────────────
      mkExtraArgs = system: username: {
        dotfiles = ./.;
        inherit username;
      };
      mkDarwinConfig =
        username: hostpath:
        let
          system = "aarch64-darwin";
        in
        nix-darwin.lib.darwinSystem {
          inherit system;
          specialArgs = mkExtraArgs system username;
          modules = [
            { nixpkgs.overlays = [ overlay ]; }
            hostpath
            ./modules/darwin.nix
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                backupFileExtension = "backup";
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = mkExtraArgs system username;
                users."${username}" = {
                  imports = [
                    ./home/common.nix
                    ./home/darwin.nix
                  ];
                };
              };
            }
          ];
        };
    in
    {
      darwinConfigurations = {
        "no-mans-work" = mkDarwinConfig "paul" ./hosts/darwin-work.nix;
        "no-mans-mini" = mkDarwinConfig "paul" ./hosts/darwin-mini.nix;
        "no-mans-land" = mkDarwinConfig "paulgrillenberger" ./hosts/darwin-home-laptop.nix;
      };

      # ── NixOS ──────────────────────────────────────────────────────────
      nixosConfigurations."home-laptop" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = mkExtraArgs "x86_64-linux" "paulgrillenberger";
        modules = [
          { nixpkgs.overlays = [ overlay ]; }
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
    };
}
