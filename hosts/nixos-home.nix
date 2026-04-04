{ pkgs, username, ... }:

{
  # ── Host identity ───────────────────────────────────────────────────────
  networking.hostName = "home-laptop";

  # ── Boot (adjust to your hardware — generate with nixos-generate-config) ─
  # imports = [ ./hardware-configuration.nix ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  # ── Locale ──────────────────────────────────────────────────────────────
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "de_DE.UTF-8";

  # ── Users ───────────────────────────────────────────────────────────────
  users.users.${username} = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
    ];
    shell = pkgs.zsh;
  };

  # ── Desktop (uncomment what you need) ───────────────────────────────────
  # services.xserver.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;

  # ── Programs ────────────────────────────────────────────────────────────
  programs.zsh.enable = true;

  # ── Nix settings ────────────────────────────────────────────────────────
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "25.05";
}
