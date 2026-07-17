{
  pkgs,
  username,
  ...
}:

{
  home = {
    # NixOS-specific home-manager settings

    inherit username;
    homeDirectory = "/home/${username}";

    packages = with pkgs; [
      vim-full
      claude-code
    ];
  };
}
