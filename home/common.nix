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
    ./kitty
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
      btop
      dust
      tealdeer
      yazi

      # Development tools
      jq
      just
      lazygit
      k9s
      shellcheck
      shfmt
      prek
      mermaid-cli

      # Infrastructure / DevOps
      terraform
      terraform-docs
      tflint
      fluxcd
      kubectl
      kubectx
      kustomize
      yq-go
      age
      sops
      awscli2

      # Languages
      nodejs
      go
      uv
      prettier
      ruff

      # Linters / formatters (used by AI coding tool hooks)
      nixfmt
      statix
      nil
      go-tools # staticcheck
      hlint
      fourmolu

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

    ];

    # ── Dotfiles (replaces dotbot symlinks) ─────────────────────────────────
    file = {
      # Neovim — out-of-store symlink so lazyvim.json / lazy-lock.json stay writable
      ".config/nvim".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/nvim";
    };

    sessionPath = [
      "$HOME/.cargo/bin" # rustup-managed Rust toolchain
      "$HOME/.ghcup/bin" # ghcup-managed Haskell toolchain
      "$HOME/.cabal/bin" # cabal-installed packages
      "$HOME/.local/bin" # ad hoc installed packages
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
