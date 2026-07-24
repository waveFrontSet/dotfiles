{
  username,
  ...
}:

{
  home = {
    # NixOS-specific home-manager settings

    inherit username;
    homeDirectory = "/home/${username}";
  };
}
