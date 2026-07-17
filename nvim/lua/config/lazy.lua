-- When set (by home-manager's neovim wrapper), all plugins come from the nix
-- store and lazy.nvim only loads them; otherwise fall back to self-managed
-- lazy.nvim (git bootstrap + runtime installs) for non-nix machines.
local nix_plugins = vim.env.NVIM_NIX_LAZY_PATH

if nix_plugins then
  vim.opt.rtp:prepend(nix_plugins .. "/lazy.nvim")
else
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
      vim.api.nvim_echo({
        { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
        { out, "WarningMsg" },
        { "\nPress any key to exit..." },
      }, true, {})
      vim.fn.getchar()
      os.exit(1)
    end
  end
  vim.opt.rtp:prepend(lazypath)
end

require("lazy").setup({
  spec = {
    -- add LazyVim and import its plugins
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },
    -- import/override with your plugins
    { import = "plugins" },
  },
  defaults = {
    -- By default, only LazyVim plugins will be lazy-loaded. Your custom plugins will load during startup.
    -- If you know what you're doing, you can set this to `true` to have all your custom plugins lazy-loaded by default.
    lazy = false,
    -- It's recommended to leave version=false for now, since a lot the plugin that support versioning,
    -- have outdated releases, which may break your Neovim install.
    version = false, -- always use the latest git commit
    -- version = "*", -- try installing the latest stable version for plugins that support semver
  },
  -- treat every plugin as a "dev" plugin resolved from the nix link farm;
  -- fallback=false so a plugin missing from home/neovim.nix fails loudly
  dev = nix_plugins and { path = nix_plugins, patterns = { "" }, fallback = false } or nil,
  install = { missing = nix_plugins == nil, colorscheme = { "tokyonight", "habamax" } },
  checker = {
    enabled = nix_plugins == nil, -- update checks only when self-managed
    notify = false, -- notify on update
  },
  -- lockfile is meaningless with nix-pinned plugins; keep it out of the repo
  lockfile = vim.fn.stdpath("state") .. "/lazy-lock.json",
  performance = {
    rtp = {
      -- extra dirs that survive lazy's rtp reset (nix treesitter parsers)
      paths = vim.env.NVIM_NIX_TS_PARSERS and { vim.env.NVIM_NIX_TS_PARSERS } or {},
      -- disable some rtp plugins
      disabled_plugins = {
        "gzip",
        -- "matchit",
        -- "matchparen",
        -- "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
})
