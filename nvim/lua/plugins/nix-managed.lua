-- Overrides that apply only when plugins come from nix (see config/lazy.lua).
if not vim.env.NVIM_NIX_LAZY_PATH then
  return {}
end

return {
  -- LSP/formatter/linter binaries come from nix (home/neovim.nix
  -- extraPackages), not from mason downloads
  { "mason-org/mason.nvim", enabled = false },
  { "mason-org/mason-lspconfig.nvim", enabled = false },

  -- parsers/queries are prebuilt by nix and reach the rtp via
  -- performance.rtp.paths; never compile or network-install them
  {
    "nvim-treesitter/nvim-treesitter",
    build = false,
    -- function form: replaces ensure_installed instead of list-merging
    opts = function(_, opts)
      opts.ensure_installed = {}
    end,
  },
  { "nvim-treesitter/nvim-treesitter-textobjects", build = false },

  -- nixpkgs ships these prebuilt (JS app / rust fuzzy matcher)
  { "iamcco/markdown-preview.nvim", build = false },
  { "saghen/blink.cmp", build = false },
}
