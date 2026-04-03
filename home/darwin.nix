{
  config,
  pkgs,
  lib,
  username,
  ...
}:

{
  home = {
    # macOS-specific home-manager settings

    inherit username;
    homeDirectory = "/Users/${username}";

    packages = with pkgs; [
      reattach-to-user-namespace
      pinentry_mac
    ];
  };
  programs.zsh.profileExtra = ''
    # Homebrew shell environment (macOS only)
    eval "$(/opt/homebrew/bin/brew shellenv)"
  '';

}
