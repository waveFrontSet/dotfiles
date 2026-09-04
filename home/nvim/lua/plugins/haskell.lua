-- nixpkgs HLS (devenv) ships only haskell-language-server-<ghcver> and
-- haskell-language-server-wrapper; ghcup used to provide the unversioned
-- binary that haskell-tools' auto-attach check looks for. Point it at the
-- wrapper (auto_attach evaluates this cmd, so this fixes both).
vim.g.haskell_tools = {
  hls = {
    cmd = { "haskell-language-server-wrapper", "--lsp" },
  },
}

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      inlay_hints = {
        enabled = true,
        exclude = { "cabal" },
      },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    optional = true,
    specs = {
      {
        "luc-tielen/telescope_hoogle",
        ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
        config = function()
          LazyVim.on_load("telescope.nvim", function()
            require("telescope").load_extension("hoogle")
          end)
        end,
        keys = {
          {
            "<localleader>H",
            "<cmd>Telescope hoogle<cr>",
            ft = "haskell",
            desc = "Hoogle",
          },
        },
      },
    },
  },
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        haskell = { "fourmolu" },
        cabal = { "cabal_fmt" },
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        haskell = { "hlint" },
      },
    },
  },
}
