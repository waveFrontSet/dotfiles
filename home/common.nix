{ config, pkgs, lib, pkgs-talos, dotfiles, ... }:

{
  home.stateVersion = "25.11";

  # allowUnfree is set at the system level when used via nix-darwin/NixOS.
  # For standalone home-manager, set it in the homeConfigurations block.

  # ── Packages (cross-platform) ───────────────────────────────────────────
  home.packages = with pkgs; [
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
    git
    vim
    jq
    just
    direnv
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
      runtimeInputs = [ fzf nix-search-tv ];
      text = builtins.readFile "${pkgs.nix-search-tv.src}/nixpkgs.sh";
    })

    # Fonts
    nerd-fonts.meslo-lg
    fira-code

    # Coding assistants
    opencode
    gemini-cli
    claude-code
  ];

  # ── Programs with modules ───────────────────────────────────────────────
  programs.home-manager.enable = true;
  programs.neovim.enable = true;

  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile (dotfiles + "/kitty/.config/kitty/kitty.conf");
  };
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    history.size = 50000;
    setOptions = [
      "AUTO_CD" # cd by typing directory name
      "AUTO_PUSHD" # push old directory onto stack
      "PUSHD_IGNORE_DUPS" # don't push duplicates
      "PUSHD_SILENT" # don't print directory stack
      "PUSHD_TO_HOME" # pushd with no args goes to ~
      "CDABLE_VARS" # try expanding as variable before directory
      "EXTENDED_GLOB" # extended globbing (#, ~, ^)
      "BANG_HIST" # treat ! specially in expansion
      "EXTENDED_HISTORY" # write timestamps to history
      "SHARE_HISTORY" # share history between sessions
      "HIST_EXPIRE_DUPS_FIRST"
      "HIST_IGNORE_DUPS"
      "HIST_IGNORE_ALL_DUPS"
      "HIST_FIND_NO_DUPS"
      "HIST_IGNORE_SPACE" # don't record commands starting with space
      "HIST_SAVE_NO_DUPS"
      "HIST_VERIFY" # show expanded history before executing
      "HIST_BEEP"
      # General
      "INTERACTIVE_COMMENTS" # allow comments in interactive shells
      "COMBINING_CHARS" # handle combining characters correctly
      "NO_MAIL_WARNING"
      "NO_BEEP"
    ];
    shellAliases = {
      gcal = "gcalcli";
      grep = "rg";
      j = "z";
      la = "ll -A";
      ll = "eza -la --icons --git";
      ls = "eza --icons";
      ssh = "kitty +kitten ssh";
      tree = "eza --tree --icons";
    };
    initContent = lib.mkMerge [
      (lib.mkOrder 1000 ''
        # Must use zvm_after_init hook because zsh-vi-mode rebinds all keys on init
        zvm_after_init_commands+=('bindkey "^ " autosuggest-accept')
        zvm_after_init_commands+=('eval "$(atuin init zsh --disable-up-arrow)"')
      '')
    ];
    # dotDir = "/";
    antidote = {
      enable = true;
      plugins = [
        # Completion system (must load before other plugins that use completion)
        "mattmc3/ez-compinit"
        "zsh-users/zsh-completions kind:fpath"

        # fzf-tab (must load after compinit but before other plugins that wrap widgets)
        "Aloxaf/fzf-tab"

        # Better vi mode for zsh
        "jeffreytse/zsh-vi-mode"

        # Prezto aliases — git (gco, gb, gws, etc.) and docker (dkcU, dkc, etc.)
        "sorin-ionescu/prezto path:modules/git/functions kind:fpath"
        "sorin-ionescu/prezto path:modules/git/alias.zsh"
        "sorin-ionescu/prezto path:modules/docker/alias.zsh"

        # Autosuggestions
        "zsh-users/zsh-autosuggestions"

        # Syntax highlighting (load near end)
        "zdharma-continuum/fast-syntax-highlighting kind:defer"
      ];
    };
  };
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.eza = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  # ── Dotfiles (replaces dotbot symlinks) ─────────────────────────────────
  home.file = {
    # Nix
    ".config/nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';

    # Git
    ".gitconfig".source = "${dotfiles}/git/.gitconfig";
    ".gitignore_global".source = "${dotfiles}/git/.gitignore_global";

    # Kitty — extraConfig is used via programs.kitty; additional files go here
    ".config/kitty/diff.conf".source = "${dotfiles}/kitty/.config/kitty/diff.conf";
    ".config/kitty/current-theme.conf".source = "${dotfiles}/kitty/.config/kitty/current-theme.conf";

    # Neovim
    ".config/nvim".source = "${dotfiles}/nvim";

    # Vim
    ".vim".source = "${dotfiles}/vim/.vim";
    ".vimrc".source = "${dotfiles}/vim/.vimrc";
    ".gvimrc".source = "${dotfiles}/vim/.gvimrc";
    ".vimpagerrc".source = "${dotfiles}/vim/.vimpagerrc";

    # Starship
    ".config/starship.toml".source = "${dotfiles}/starship/.config/starship.toml";

    # Direnv
    ".direnvrc".source = "${dotfiles}/direnv/.direnvrc";

    # Doom Emacs
    ".doom.d".source = "${dotfiles}/emacs/.doom.d";

    # LaTeX
    ".latexmkrc".source = "${dotfiles}/latexmk/.latexmkrc";

    # fsh (zsh syntax highlighting)
    ".config/fsh".source = "${dotfiles}/fsh";

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

  home.sessionPath = [
    "$HOME/.cargo/bin" # rustup-managed Rust toolchain
    "$HOME/.ghcup/bin" # ghcup-managed Haskell toolchain
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
    LESS = "-F -g -i -M -R -S -w -X -z-4";
    DOCKER_DEFAULT_PLATFORM = "linux/amd64";
  };

}
