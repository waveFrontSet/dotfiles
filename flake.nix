{
  description = "Home Manager configuration of paulgrillenberger";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-talos.url = "github:NixOS/nixpkgs/6a43094a5d05d6f0e2232d340baa9bae555ef232";
  };

  outputs =
    { nixpkgs, nixpkgs-talos, home-manager, ... }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      pkgs-talos = import nixpkgs-talos {inherit system;};
    in
    {
      homeConfigurations."paulgrillenberger" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = { inherit pkgs-talos; };
        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };
    };
}
