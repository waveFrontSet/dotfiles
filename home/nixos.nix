{ config, pkgs, lib, username, ... }:

{
  # NixOS-specific home-manager settings

  home.username = username;
  home.homeDirectory = "/home/${username}";
}
