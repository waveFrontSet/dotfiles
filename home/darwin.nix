{ config, pkgs, lib, ... }:

{
  # macOS-specific home-manager settings

  home.username = "paulgrillenberger";
  home.homeDirectory = "/Users/paulgrillenberger";

  home.packages = with pkgs; [
    reattach-to-user-namespace
    pinentry_mac
  ];
}
