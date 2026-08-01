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

    # Third-party skills
    allium = {
      url = "github:juxt/allium";
      flake = false;
    };

    # Vim plugins missing from nixpkgs (built via vimUtils.buildVimPlugin)
    vim-latex = {
      url = "github:jcf/vim-latex";
      flake = false;
    };
    tokyonight-vim = {
      url = "github:ghifarit53/tokyonight-vim";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nix-darwin,
      allium,
      vim-latex,
      tokyonight-vim,
      ...
    }:
    let
      # ── Overlay to pin specific package versions ────────────────────────
      overlay = import ./overlays;

      # ── Shared extra args passed to every module ────────────────────────
      mkExtraArgs = username: {
        dotfiles = ./.;
        inherit username;
        skills = { inherit allium; };
        vimPluginSrcs = {
          inherit
            vim-latex
            tokyonight-vim
            ;
        };
      };
      mkDarwinConfig =
        username: hostpath:
        let
          system = "aarch64-darwin";
        in
        nix-darwin.lib.darwinSystem {
          inherit system;
          specialArgs = mkExtraArgs username;
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
                extraSpecialArgs = mkExtraArgs username;
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
      # ── `nix fmt` ──────────────────────────────────────────────────────
      formatter = {
        aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixfmt-tree;
        x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-tree;
      };

      darwinConfigurations = {
        "no-mans-work" = mkDarwinConfig "paul" ./hosts/darwin-work.nix;
        "no-mans-mini" = mkDarwinConfig "paul" ./hosts/darwin-mini.nix;
        "no-mans-land" = mkDarwinConfig "paulgrillenberger" ./hosts/darwin-home-laptop.nix;
      };

      # ── NixOS ──────────────────────────────────────────────────────────
      nixosConfigurations."home-laptop" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = mkExtraArgs "paulgrillenberger";
        modules = [
          { nixpkgs.overlays = [ overlay ]; }
          ./hosts/nixos-home.nix
          ./modules/nixos.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = mkExtraArgs "paulgrillenberger";
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
