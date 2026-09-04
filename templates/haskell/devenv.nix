_: {
  languages.haskell = {
    enable = true;
    stack.enable = false;
  };

  git-hooks.hooks = {
    fourmolu = {
      enable = true;
      package = null;
      entry = "fourmolu --mode inplace"; # globally installed overlay pin
    };
    hlint.enable = true;
  };
}
