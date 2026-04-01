{ config, pkgs, lib, username, ... }:

{
  # macOS-specific home-manager settings

  home.username = username;
  home.homeDirectory = "/Users/${username}";

  home.packages = with pkgs; [
    reattach-to-user-namespace
    pinentry_mac
  ];
  programs.zsh.profileExtra = ''
    # Homebrew shell environment (macOS only)
    eval "$(/opt/homebrew/bin/brew shellenv)"
  '';

}
