{
  pkgs,
  username,
  pkgs-claude-code,
  ...
}:

{
  home = {
    # NixOS-specific home-manager settings

    inherit username;
    homeDirectory = "/home/${username}";

    packages = with pkgs; [
      pkgs-claude-code.claude-code
    ];
  };
}
