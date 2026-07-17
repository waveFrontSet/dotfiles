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

  # darwin-specific vim base for the nix-managed plugin setup (see vim.nix)
  programs.vim.packageConfigurable = pkgs.vim-darwin;
}
