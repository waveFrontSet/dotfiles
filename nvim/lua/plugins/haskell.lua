return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      inlay_hints = {
        enabled = true,
        exclude = { "cabal" },
      },
      servers = {
        hls = {
          root_dir = require("lspconfig.util").root_pattern("stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
        },
      },
    },
  },
}
