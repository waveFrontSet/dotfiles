{
  config,
  pkgs,
  ...
}:

{
  imports = [
    ./agents
    ./direnv.nix
    ./gh.nix
    ./git.nix
    ./kitty.nix
    ./nvim
    ./vim
    ./zsh.nix
  ];
  home = {
    stateVersion = "25.11";

    # allowUnfree is set at the system level when used via nix-darwin/NixOS.
    # For standalone home-manager, set it in the homeConfigurations block.

    # ── Packages (cross-platform) ───────────────────────────────────────────
    packages = with pkgs; [
      talosctl

      # Core utilities
      coreutils
      findutils
      bash
      less
      wget

      # Modern CLI replacements
      bat
      fd
      ripgrep
      delta
      atuin

      # Development tools
      jq
      just
      lazygit
      k9s
      shellcheck
      shfmt
      prek

      # Infrastructure / DevOps
      terraform
      terraform-docs
      tflint
      fluxcd
      kustomize
      age
      sops
      awscli2

      # Languages
      nodejs
      go
      uv

      # Linters / formatters (used by AI coding tool hooks)
      nixfmt
      statix
      nil
      go-tools # staticcheck
      hlint

      # Utilities
      hugo
      pandoc
      aspell
      html2text
      lynx
      oha
      pigz
      gnupg
      keychain
      markdownlint-cli2
      msmtp
      gcalcli
      nix-search-tv
      (pkgs.writeShellApplication {
        name = "ns";
        runtimeInputs = [
          fzf
          nix-search-tv
        ];
        text = builtins.readFile "${pkgs.nix-search-tv.src}/nixpkgs.sh";
      })

      # Fonts
      nerd-fonts.meslo-lg
      fira-code

      # Coding assistants
      opencode
      gemini-cli
      pi-coding-agent
    ];

    # ── Dotfiles (replaces dotbot symlinks) ─────────────────────────────────
    file = {
      # Nix
      ".config/nix/nix.conf".text = ''
        experimental-features = nix-command flakes
      '';

      # Neovim — out-of-store symlink so lazyvim.json / lazy-lock.json stay writable
      ".config/nvim".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/nvim";
    };

    sessionPath = [
      "$HOME/.cargo/bin" # rustup-managed Rust toolchain
      "$HOME/.ghcup/bin" # ghcup-managed Haskell toolchain
      "$HOME/.cabal/bin" # cabal-installed packages
    ];

    sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
      LESS = "-F -g -i -M -R -S -w -X -z-4";
      DOCKER_DEFAULT_PLATFORM = "linux/amd64";
    };
  };
  programs = {
    home-manager.enable = true;
  };
}
