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
    zsh
    less
    wget

    # Modern CLI replacements
    bat
    eza
    fd
    ripgrep
    delta
    zoxide
    atuin
    fzf

    # Shell
    antidote
    starship

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
    cargo

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

    # Fonts
    nerd-fonts.meslo-lg
    fira-code
  ];

  # ── Programs with modules ───────────────────────────────────────────────
  programs.home-manager.enable = true;
  programs.neovim.enable = true;

  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile (dotfiles + "/kitty/.config/kitty/kitty.conf");
  };

  # ── Dotfiles (replaces dotbot symlinks) ─────────────────────────────────
  home.file = {
    # Nix
    ".config/nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';

    # Shell
    ".zshrc".source = "${dotfiles}/zsh/.zshrc";
    ".zshenv".source = "${dotfiles}/zsh/.zshenv";
    ".zprofile".source = "${dotfiles}/zsh/.zprofile";
    ".zlogin".source = "${dotfiles}/zsh/.zlogin";
    ".zlogout".source = "${dotfiles}/zsh/.zlogout";
    ".zsh_plugins.txt".source = "${dotfiles}/zsh/.zsh_plugins.txt";

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
  };

  home.sessionVariables = {
    # EDITOR = "nvim";
  };
}
