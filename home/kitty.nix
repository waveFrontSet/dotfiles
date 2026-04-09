# Home-manager configuration for kitty
{ ... }:
{
  home.file = {
    ".config/kitty-sessions/dotfiles.kitty-session".source =
      .config/kitty-sessions/dotfiles.kitty-session;
  };
  programs.kitty = {
    enable = true;
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
      tab_bar_style = "powerline";
      tab_powerline_style = "round";
      tab_bar_min_tabs = 1;
      tab_activity_symbol = "⚠️";
      macos_option_as_alt = "left";
      macos_thicken_font = 0.0;
      scrollback_lines = 5000;
      scrollback_pager = "less --chop-long-lines --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER";
      enabled_layouts = "Fat,Stack";
    };
    keybindings = {
      "cmd+g" = "goto_session ~/.config/kitty-sessions/";
      "f1" = "save_as_session --use-foreground-process --relocatable";
    };
  };
}
