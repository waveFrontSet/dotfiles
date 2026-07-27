_: {
  home.file = {
    ".config/kitty-sessions/dotfiles.kitty-session".source = ./sessions/dotfiles.kitty-session;
    ".config/kitty/tab_bar.py".source = ./tab_bar.py;
    ".config/kitty/presentation_mode.py".source = ./presentation_mode.py;
  };
  programs.kitty = {
    enable = true;
    # Binary comes from the homebrew cask (Developer ID signed, so macOS
    # notifications work); home-manager only manages the config.
    package = null;
    enableGitIntegration = true;
    font = {
      name = "family=\"Meslo LG S\"";
      size = 16.0;
    };
    themeFile = "tokyo_night_storm";
    settings = {
      hide_window_decorations = "yes";
      tab_bar_edge = "top";
      tab_bar_margin_width = 0.0;
      tab_bar_style = "custom";
      tab_powerline_style = "round";
      tab_bar_min_tabs = 1;
      tab_activity_symbol = "•";
      bell_on_tab = "🔔 ";
      tab_title_template = "{fmt.fg.orange}{bell_symbol}{activity_symbol}{fmt.fg.tab}{tab.last_focused_progress_percent}{session_name or title}";
      active_tab_title_template = "{fmt.fg.tab}{tab.last_focused_progress_percent}{session_name or title}";
      progress_bar = "top";
      notify_on_cmd_finish = "invisible 10.0 notify-bell";
      macos_option_as_alt = "left";
      macos_thicken_font = 0.0;
      scrollback_lines = 5000;
      scrollback_pager = "less --chop-long-lines --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER";
      enabled_layouts = "Fat,Stack";
    };
    keybindings = {
      "cmd+g" = "goto_session --sort-by=alphabetical ~/.config/kitty-sessions/";
      "cmd+l" = "next_layout";
      "cmd+]" = "next_window";
      "cmd+[" = "previous_window";
      "cmd+shift+p" = "kitten presentation_mode.py";
      "f1" = "save_as_session --use-foreground-process --relocatable";
    };
  };
}
