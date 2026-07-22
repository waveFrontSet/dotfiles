{
  pkgs,
  vimPluginSrcs,
  ...
}:

let
  # Plugins missing from nixpkgs, built from pinned flake inputs
  mkPlug =
    name: src:
    pkgs.vimUtils.buildVimPlugin {
      pname = name;
      version = src.shortRev or "src";
      inherit src;
    };
in
{
  # The customized vim wrapper runs `vim -u <generated vimrc>`, so ~/.vimrc
  # is no longer read; vimrc changes require a rebuild. ~/.vim stays on the
  # rtp (personal colors/, syntax/, ftdetect/ keep working).
  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-airline
      vim-airline-themes
      tabular
      vim-indent-object
      ale
      gundo-vim
      vim-exchange
      vim-commentary
      vim-fugitive
      vim-repeat
      vim-surround
      vim-unimpaired
      vim-rails
      ultisnips
      vim-terraform
      (mkPlug "tokyonight-vim" vimPluginSrcs.tokyonight-vim)
      (mkPlug "vim-latex" vimPluginSrcs.vim-latex)
    ];
    extraConfig = builtins.readFile ./vimrc;
  };
}
