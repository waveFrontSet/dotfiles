{
  pkgs,
  lib,
  config,
  ...
}:

let
  # Lid closed ⇒ no Touch ID and no terminal prompt, so on macOS route sudo
  # through the GUI askpass helper from modules/darwin.nix.
  sudo =
    if pkgs.stdenv.hostPlatform.isDarwin then "SUDO_ASKPASS=/etc/sudo-askpass sudo -A" else "sudo";
in
{
  # Minimal — the rebuilders (darwin-rebuild / nixos-rebuild) come from the
  # system profile, and `nix` from the global Nix installation.
  packages = [ pkgs.git ];

  tasks = lib.mkMerge [
    (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
      # One-time bootstrap of nix-darwin (before darwin-rebuild is installed).
      "bootstrap:main" = {
        exec = "sudo nix run --experimental-features 'nix-command flakes' github:LnL7/nix-darwin#darwin-rebuild -- switch --flake ${config.git.root}";
        status = "command -v darwin-rebuild";
      };

      # Build & switch the macOS config (nix-darwin + home-manager).
      "switch:main" = {
        exec = "${sudo} darwin-rebuild switch --flake ${config.git.root}";
      };
    })

    (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      # Build & switch the NixOS config.
      "switch:main" = {
        exec = "sudo nixos-rebuild switch --flake ${config.git.root}#home-laptop";
      };
    })

    {
      # Update flake inputs (nixpkgs, home-manager, nix-darwin), then rebuild.
      "update:main" = {
        exec = "nix flake update && devenv tasks run switch:main";
      };

      # Drop system generations older than 30 days, then collect garbage.
      "gc:main" = {
        exec = ''
          ${sudo} nix profile wipe-history --profile /nix/var/nix/profiles/system --older-than 30d
          nix store gc
          nix store optimise
        '';
      };
    }
  ];

  git-hooks.hooks = {
    nixfmt.enable = true;
    statix = {
      enable = true;
      pass_filenames = false;
    };
    markdownlint = {
      enable = true;
      files = "^README\\.md$";
      settings.configuration.MD013 = {
        tables = false;
        code_blocks = false;
      };
    };
  };
}
