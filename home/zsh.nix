{ lib, ... }:
{
  home.file = {
    # Starship
    ".config/starship.toml".source = .config/starship.toml;
    # fast syntax highlighting theme
    ".config/fsh".source = .config/fsh;
  };
  programs = {
    zsh = {
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
    starship = {
      enable = true;
      enableZshIntegration = true;
    };
    eza = {
      enable = true;
      enableZshIntegration = true;
      theme = "tokyonight";
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}
