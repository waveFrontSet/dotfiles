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

    # Vim plugins missing from nixpkgs (built via vimUtils.buildVimPlugin)
    vim-latex = {
      url = "github:jcf/vim-latex";
      flake = false;
    };
    tokyonight-vim = {
      url = "github:ghifarit53/tokyonight-vim";
      flake = false;
    };
    tokyonight-yazi = {
      url = "github:BennyOe/tokyo-night.yazi";
      flake = false;
    };

    # Language .gitignore templates for the devi-* scaffolding helpers
    gitignore = {
      url = "github:github/gitignore";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nix-darwin,
      vim-latex,
      tokyonight-vim,
      tokyonight-yazi,
      gitignore,
      ...
    }:
    let
      # ── Overlay to pin specific package versions ────────────────────────
      overlay = import ./overlays;

      # ── Shared extra args passed to every module ────────────────────────
      mkExtraArgs = username: {
        dotfiles = ./.;
        inherit username;
        vimPluginSrcs = {
          inherit
            vim-latex
            tokyonight-vim
            ;
        };
        inherit tokyonight-yazi;
        inherit gitignore;
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

      # ── devenv project templates (see home/zsh.nix devi-* helpers) ─────
      templates = {
        rust = {
          path = ./templates/rust;
          description = "Rust devenv environment (stable toolchain, rustfmt + clippy commit hooks)";
        };
        haskell = {
          path = ./templates/haskell;
          description = "Haskell devenv environment (GHC, Cabal, HLS; global fourmolu)";
        };
        python = {
          path = ./templates/python;
          description = "Python devenv environment (uv + ruff commit hooks)";
        };
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
