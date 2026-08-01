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

  # darwin-specific vim base for the nix-managed plugin setup (see vim/default.nix)
  programs.vim.packageConfigurable = pkgs.vim-darwin;

  # darwin-specific git settings (base config in git.nix)
  programs.git.settings.credential.helper = "osxkeychain";
}
