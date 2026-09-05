{
  pkgs,
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

  programs = {
    # darwin-specific vim base for the nix-managed plugin setup (see vim/default.nix)
    vim.packageConfigurable = pkgs.vim-darwin;

    # darwin-specific git settings (base config in git.nix)
    git.settings.credential.helper = "osxkeychain";

    # ── Bitwarden: unlock with Touch ID ─────────────────────────────────────
    # bwbio (brew, jeanregisser/tap — see modules/darwin.nix) asks the Bitwarden
    # Desktop app to unlock via Touch ID and delegates to the real bw, so there is
    # no master password and no manual BW_SESSION juggling. The session key is
    # cached per shell: one prompt per terminal instead of one per command.
    zsh.initContent = ''
      _bw_unlock() {
        # bwbio 1.4.2 probes the Mac App Store socket first, and the stale `close`
        # event of that failed probe tears down the live connection (upstream
        # PR #4, not released yet). Hand it the real socket instead.
        local sock key
        for sock in \
          "$HOME/Library/Caches/com.bitwarden.desktop/s.bw" \
          "$HOME/Library/Group Containers/LTZ2PFU5D6.com.bitwarden.desktop/s.bw"; do
          [[ -S "$sock" ]] && export BWBIO_IPC_SOCKET_PATH="$sock" && break
        done

        key="$(bwbio unlock --raw)"
        # A session key is one ~88-char base64 line; anything else (or a non-zero
        # exit) means bwbio fell back to the interactive master-password prompt.
        if [[ $? -ne 0 || ''${#key} -lt 80 || "$key" == *$'\n'* ]]; then
          print -u2 'bw: Touch ID unlock failed — Desktop app running with biometrics?'
          print -u2 'bw: password fallback: export BW_SESSION=$(command bw unlock --raw)'
          return 1
        fi
        export BW_SESSION="$key"
      }

      bw() {
        # Commands and options that work on a locked vault: never prompt.
        case "''${1:-}" in
          login | logout | lock | config | status | completion | update | "" | -*)
            bwbio "$@"
            return
            ;;
          unlock)
            # `bw unlock --raw` must print the key (for scripts), not export it.
            if [[ "''${2:-}" == "--raw" ]]; then bwbio "$@"; else _bw_unlock; fi
            return
            ;;
        esac

        # A vault timeout or `bw lock` invalidates the cached key: re-unlock then.
        local vault="" # `status` is read-only in zsh
        [[ -n "''${BW_SESSION:-}" ]] && vault="$(bwbio status --raw 2>/dev/null)"
        if [[ -z "$vault" || "$vault" == *'"locked"'* ]]; then
          _bw_unlock || return
        fi
        bwbio "$@"
      }
    '';
  };
}
