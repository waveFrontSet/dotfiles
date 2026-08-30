{
  lib,
  pkgs,
  username,
  ...
}:

{
  # ── Nix settings ────────────────────────────────────────────────────────
  # Determinate Nix manages the daemon; disable nix-darwin's Nix management.
  nix.enable = false;
  nixpkgs.config.allowUnfree = true;
  system = {

    # ── User ─────────────────────────────────────────────────────────────────
    primaryUser = username;

    # ── macOS system defaults ───────────────────────────────────────────────
    # These replace the most important parts of the osx/ shell scripts.
    # For the full set, you can still run: just macos / just macos-laptop
    defaults = {
      controlcenter = {
        AirDrop = false;
        Bluetooth = false;
        BatteryShowPercentage = true;
        Display = false;
        FocusModes = false;
        NowPlaying = false;
        Sound = false;
      };
      dock = {
        autohide = true;
        autohide-delay = 0.0;
        autohide-time-modifier = 0.0;
        expose-animation-duration = 0.1;
        launchanim = false;
        mru-spaces = false;
        show-process-indicators = true;
        showhidden = true;
        tilesize = 36;
      };
      dock.persistent-apps = [
        "/Applications/Brave Browser.app"
        "/Applications/kitty.app"
        "/Applications/Signal.app"
        "/Applications/Bitwarden.app"
        "/Applications/Bruno.app"
        "/Applications/Spotify.app"
      ];

      finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        FXDefaultSearchScope = "SCcf";
        FXEnableExtensionChangeWarning = false;
        FXPreferredViewStyle = "Nlsv";
        QuitMenuItem = true;
        ShowPathbar = true;
        ShowStatusBar = true;
        _FXShowPosixPathInTitle = true;
      };

      NSGlobalDomain = {
        AppleKeyboardUIMode = 3;
        ApplePressAndHoldEnabled = false;
        AppleShowAllExtensions = true;
        KeyRepeat = 2;
        InitialKeyRepeat = 15;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSAutomaticWindowAnimationsEnabled = false;
        NSDocumentSaveNewDocumentsToCloud = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSTableViewDefaultSizeMode = 2;
        PMPrintingExpandedStateForPrint = true;
      };

      trackpad = {
        Clicking = true;
        TrackpadRightClick = true;
        TrackpadThreeFingerDrag = false;
      };

      screencapture.location = "~/Desktop";
      screencapture.type = "png";

      loginwindow.GuestEnabled = false;

      CustomUserPreferences = {
        "com.apple.desktopservices" = {
          DSDontWriteNetworkStores = true;
        };
        "com.apple.LaunchServices" = {
          LSQuarantine = false;
        };
      };
    };

    stateVersion = 6;
  };
  users.users.${username} = {
    home = "/Users/${username}";
    shell = pkgs.zsh;
    uid = lib.mkDefault 501;
  };
  users.knownUsers = [ username ];

  programs.zsh.enable = true;
  environment.shells = [
    pkgs.zsh
    "/etc/profiles/per-user/${username}/bin/zsh"
  ];

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      cleanup = "zap"; # remove unlisted casks/formulae
      upgrade = true;
    };

    taps = [
      # Add taps for formulae that remain in brew
    ];

    brews = [
      # Add brews for formulae that remain in brew
    ];

    casks = [
      "amethyst"
      "bitwarden"
      "brave-browser"
      "bruno"
      "devpod"
      "docker-desktop"
      "drawio"
      "font-fira-code"
      "kitty"
      "menumeters"
      "signal"
      "spotify"
      "vlc"
    ];
    enableZshIntegration = true;
  };

  security.pam.services.sudo_local.touchIdAuth = true;
}
