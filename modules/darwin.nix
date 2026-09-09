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
      {
        name = "jeanregisser/tap";
        trusted = true;
      }
    ];

    brews = [
      "bitwarden-cli"
      "bitwarden-cli-bio"
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
  # Retain this for Homebrew's nested sudo calls during system activation.
  security.sudo.extraConfig = ''
    Defaults env_keep += "SUDO_ASKPASS"
  '';

  # GUI password prompt for `sudo -A` (see devenv.nix tasks): Touch ID is
  # unavailable with the lid closed, so sudo needs a graphical askpass.
  environment.etc."sudo-askpass".source = pkgs.writeShellScript "sudo-askpass" ''
    exec /usr/bin/osascript -e 'text returned of (display dialog "Password for the devenv system task:" with title "sudo" with icon caution with hidden answer default answer "")'
  '';
}
