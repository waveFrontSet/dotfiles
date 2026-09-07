{
  pkgs,
  lib,
  config,
  ...
}:

let
  # On macOS only fall back to the GUI askpass helper from modules/darwin.nix
  # when the lid is closed (no Touch ID, no visible terminal); otherwise use
  # plain sudo so Touch ID works. A script, not a shell function: the task
  # execs embed it as a command prefix ("${sudo} <cmd>").
  sudo =
    if pkgs.stdenv.hostPlatform.isDarwin then
      toString (
        pkgs.writeShellScript "sudo-lid-aware" ''
          if /usr/sbin/ioreg -r -k AppleClamshellState -d 1 | /usr/bin/grep -q '"AppleClamshellState" = Yes'; then
            SUDO_ASKPASS=/etc/sudo-askpass exec /usr/bin/sudo -A "$@"
          else
            exec /usr/bin/sudo "$@"
          fi
        ''
      )
    else
      "sudo";
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
