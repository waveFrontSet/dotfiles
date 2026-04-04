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

}
