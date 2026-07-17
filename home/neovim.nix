{
  config,
  pkgs,
  lib,
  ...
}:

let
  # Lazy plugin name -> nixpkgs package. The single place to maintain:
  # every plugin lazy.nvim loads must have an entry here (dev.fallback is
  # disabled, so a missing entry fails loudly at startup).
  lazyPlugins = with pkgs.vimPlugins; {
    "LazyVim" = LazyVim;
    "LuaSnip" = luasnip;
    "SchemaStore.nvim" = SchemaStore-nvim;
    "blink.cmp" = blink-cmp;
    "blink.compat" = blink-compat;
    "bufferline.nvim" = bufferline-nvim;
    "catppuccin" = catppuccin-nvim;
    "claudecode.nvim" = claudecode-nvim;
    "conform.nvim" = conform-nvim;
    "crates.nvim" = crates-nvim;
    "flash.nvim" = flash-nvim;
    "friendly-snippets" = friendly-snippets;
    "gh.nvim" = gh-nvim;
    "gitsigns.nvim" = gitsigns-nvim;
    "grug-far.nvim" = grug-far-nvim;
    "haskell-snippets.nvim" = haskell-snippets-nvim;
    "haskell-tools.nvim" = haskell-tools-nvim;
    "helm-ls.nvim" = helm-ls-nvim;
    "lazy.nvim" = lazy-nvim;
    "lazydev.nvim" = lazydev-nvim;
    "litee.nvim" = litee-nvim;
    "lualine.nvim" = lualine-nvim;
    "markdown-preview.nvim" = markdown-preview-nvim;
    "mini.ai" = mini-ai;
    "mini.files" = mini-files;
    "mini.icons" = mini-icons;
    "mini.pairs" = mini-pairs;
    "neotest" = neotest;
    "neotest-golang" = neotest-golang;
    "neotest-haskell" = neotest-haskell;
    "neotest-python" = neotest-python;
    "noice.nvim" = noice-nvim;
    "nui.nvim" = nui-nvim;
    "nvim-lint" = nvim-lint;
    "nvim-lspconfig" = nvim-lspconfig;
    "nvim-nio" = nvim-nio;
    "nvim-treesitter" = nvim-treesitter;
    "nvim-treesitter-textobjects" = nvim-treesitter-textobjects;
    "nvim-ts-autotag" = nvim-ts-autotag;
    "persistence.nvim" = persistence-nvim;
    "plenary.nvim" = plenary-nvim;
    "render-markdown.nvim" = render-markdown-nvim;
    "rustaceanvim" = rustaceanvim;
    "sidekick.nvim" = sidekick-nvim;
    "snacks.nvim" = snacks-nvim;
    "supermaven-nvim" = supermaven-nvim;
    "todo-comments.nvim" = todo-comments-nvim;
    "tokyonight.nvim" = tokyonight-nvim;
    "trouble.nvim" = trouble-nvim;
    "ts-comments.nvim" = ts-comments-nvim;
    "venv-selector.nvim" = venv-selector-nvim;
    "vim-dadbod" = vim-dadbod;
    "vim-dadbod-completion" = vim-dadbod-completion;
    "vim-dadbod-ui" = vim-dadbod-ui;
    "vimtex" = vimtex;
    "which-key.nvim" = which-key-nvim;
  };

  lazyPath = pkgs.linkFarm "lazy-nvim-plugins" (
    lib.mapAttrsToList (name: path: { inherit name path; }) lazyPlugins
  );

  # Prebuilt treesitter parsers + queries; kept on the rtp via
  # performance.rtp.paths in nvim/lua/config/lazy.lua.
  treesitterParsers = pkgs.symlinkJoin {
    name = "nvim-treesitter-parsers";
    paths = pkgs.vimPlugins.nvim-treesitter.withAllGrammars.passthru.dependencies;
  };
in
{
  programs.neovim = {
    enable = true;
    sideloadInitLua = true;
    withPython3 = false;
    withRuby = false;
    # LSP servers / formatters / linters that mason used to download.
    # nil, nixfmt, statix, shellcheck, shfmt, hlint, tflint, terraform and
    # markdownlint-cli2 are already in home.packages; hls comes from ghcup,
    # rust-analyzer from rustup (both on sessionPath).
    extraPackages = with pkgs; [
      lua-language-server
      stylua
      basedpyright
      ruff
      mypy
      gopls
      gofumpt
      gotools
      yaml-language-server
      vscode-langservers-extracted
      dockerfile-language-server
      docker-compose-language-service
      hadolint
      helm-ls
      marksman
      terraform-ls
      texlab
      taplo
      sqlfluff
      fourmolu
      haskellPackages.cabal-fmt
    ];
    extraWrapperArgs = [
      "--set"
      "NVIM_NIX_LAZY_PATH"
      "${lazyPath}"
      "--set"
      "NVIM_NIX_TS_PARSERS"
      "${treesitterParsers}"
    ];
  };
}
