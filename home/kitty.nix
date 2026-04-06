# Home-manager configuration for kitty
{ dotfiles, ... }:
{
  home.file = {
    ".config/kitty/diff.conf".source = "${dotfiles}/kitty/.config/kitty/diff.conf";
    ".config/kitty/current-theme.conf".source = "${dotfiles}/kitty/.config/kitty/current-theme.conf";
  };
  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile (dotfiles + "/kitty/.config/kitty/kitty.conf");
  };
}
