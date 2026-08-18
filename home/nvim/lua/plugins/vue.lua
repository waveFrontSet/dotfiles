return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        vue_ls = {
          init_options = {
            vue = {
              hybridMode = false,
            },
          },
        },
      },
    },
  },
}
