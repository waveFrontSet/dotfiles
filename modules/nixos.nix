{ config, pkgs, lib, ... }:

{
  # ── NixOS-wide shared configuration ─────────────────────────────────────
  # Settings common to all NixOS hosts go here.
  # Host-specific config belongs in hosts/nixos-*.nix.

  # ── Networking ──────────────────────────────────────────────────────────
  networking.networkmanager.enable = true;

  # ── Docker ──────────────────────────────────────────────────────────────
  virtualisation.docker.enable = true;

  # ── Fonts ───────────────────────────────────────────────────────────────
  fonts.packages = with pkgs; [
    nerd-fonts.meslo-lg
    fira-code
  ];
}
