local flake = "builtins.getFlake (toString ./.)"
local darwin = string.format(
  'let flake = %s; in (flake.darwinConfigurations."%s" or { options = {}; }).options',
  flake,
  vim.fn.hostname()
)

return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        nix = { "nixfmt" },
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        nix = { "statix" },
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        nixd = {
          settings = {
            nixd = {
              nixpkgs = {
                expr = "import " .. flake .. ".inputs.nixpkgs { }",
              },
              options = {
                darwin = { expr = darwin },
              },
            },
          },
        },
      },
    },
  },
  -- Expose statix auto-fixes as code actions (nvim-lint only surfaces
  -- diagnostics). Base spec comes from the lsp.none-ls extra (lazyvim.json).
  {
    "nvimtools/none-ls.nvim",
    opts = function(_, opts)
      local nls = require("null-ls")
      opts.sources = vim.list_extend(opts.sources or {}, {
        nls.builtins.code_actions.statix,
      })
    end,
  },
}
