{
  config,
  pkgs,
  pkgs-talos,
  dotfiles,
  ...
}:

{
  imports = [
    ./direnv.nix
    ./git.nix
    ./kitty.nix
    ./zsh.nix
  ];
  home = {
    stateVersion = "25.11";

    # allowUnfree is set at the system level when used via nix-darwin/NixOS.
    # For standalone home-manager, set it in the homeConfigurations block.

    # ── Packages (cross-platform) ───────────────────────────────────────────
    packages = with pkgs; [
      # Pinned
      pkgs-talos.talosctl

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
      vim
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
      gh
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
    ];

    # ── Dotfiles (replaces dotbot symlinks) ─────────────────────────────────
    file = {
      # Nix
      ".config/nix/nix.conf".text = ''
        experimental-features = nix-command flakes
      '';

      # Neovim — out-of-store symlink so lazyvim.json / lazy-lock.json stay writable
      ".config/nvim".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/nvim";

      # Vim — out-of-store symlink so plugged/ / autoload/ stay writable
      ".vim".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/vim/.vim";
      ".vimrc".source = "${dotfiles}/vim/.vimrc";
      ".gvimrc".source = "${dotfiles}/vim/.gvimrc";
      ".vimpagerrc".source = "${dotfiles}/vim/.vimpagerrc";

      # OpenCode
      ".config/opencode/opencode.json".source = "${dotfiles}/opencode/.config/opencode/opencode.json";
      ".config/opencode/tui.json".source = "${dotfiles}/opencode/.config/opencode/tui.json";
      ".config/opencode/commands".source = "${dotfiles}/opencode/.config/opencode/commands";
      ".config/opencode/plugins".source = "${dotfiles}/agents/opencode/plugins";
      ".config/opencode/AGENTS.md".source = "${dotfiles}/agents/AGENTS.md";

      # Claude Code
      ".claude/settings.json".source = "${dotfiles}/agents/claude/settings.json";
      ".claude/hooks".source = "${dotfiles}/agents/claude/hooks";
      ".claude/CLAUDE.md".source = "${dotfiles}/agents/AGENTS.md";

      # Shared skills (Claude Code + OpenCode both read ~/.claude/skills/)
      ".claude/skills/review/SKILL.md".source = "${dotfiles}/agents/skills/review/SKILL.md";
      ".claude/skills/test/SKILL.md".source = "${dotfiles}/agents/skills/test/SKILL.md";
      ".claude/skills/explain/SKILL.md".source = "${dotfiles}/agents/skills/explain/SKILL.md";
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
    neovim = {
      enable = true;
      withPython3 = false;
      withRuby = false;
    };
  };

}
