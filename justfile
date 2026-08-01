# List available recipes
default:
    @just --list

# ── Nix / nix-darwin ──────────────────────────────────────────────────────

# Bootstrap nix-darwin (nix + home-manager)
[macos]
bootstrap:
    sudo nix run --experimental-features 'nix-command flakes' github:LnL7/nix-darwin#darwin-rebuild -- switch --flake {{justfile_directory()}}

# Build & switch macOS config (nix-darwin + home-manager)
[macos]
switch:
    sudo darwin-rebuild switch --flake {{justfile_directory()}}

# Build & switch NixOS config
[linux]
switch:
    sudo nixos-rebuild switch --flake {{justfile_directory()}}#home-laptop

# Update flake inputs (nixpkgs, home-manager, nix-darwin)
update:
    nix flake update --flake {{justfile_directory()}}
    just switch

# Drop system generations older than 30 days, then collect garbage
gc:
    sudo nix profile wipe-history --profile /nix/var/nix/profiles/system --older-than 30d
    nix store gc
    nix store optimise

# ── Git ───────────────────────────────────────────────────────────────────

# Install git pre-commit hooks (prek)
install-hooks:
    prek install
