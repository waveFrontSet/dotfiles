# List available recipes
default:
    @just --list

# ── Nix / nix-darwin ──────────────────────────────────────────────────────

# Bootstrap nix-darwin (nix + home-manager)
[macos]
bootstrap:
    nix run nix-darwin -- switch --flake $HOME/dotfiles#work-mbp --accept-flake-config

# Build & switch macOS config (nix-darwin + home-manager)
[macos]
switch:
    sudo darwin-rebuild switch --flake $HOME/dotfiles#work-mbp

# Build & switch NixOS config
[linux]
switch:
    sudo nixos-rebuild switch --flake $HOME/dotfiles#home-laptop

# Build & switch standalone home-manager (fallback, no nix-darwin)
hm-switch:
    home-manager switch --flake $HOME/dotfiles#paulgrillenberger

# Update flake inputs (nixpkgs, home-manager, nix-darwin)
update:
    nix flake update --flake $HOME/dotfiles
    just switch

# ── Utilities ─────────────────────────────────────────────────────────────

# Update neovim plugins
nvim-update:
    nvim --headless "+Lazy! sync" +qa
