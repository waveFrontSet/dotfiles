# Dotfiles

Personal configuration files managed with [Nix](https://nixos.org/), [home-manager](https://github.com/nix-community/home-manager), and [nix-darwin](https://github.com/LnL7/nix-darwin).

## Supported hosts

| Host | OS | Flake output |
| ---- | -- | ------------ |
| `work-mbp` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `home-laptop` | NixOS (x86_64-linux) | `nixosConfigurations` |
| fallback | any | `homeConfigurations` — standalone home-manager |

## What's managed

**Packages** — bat, eza, fd, ripgrep, fzf, delta, zoxide, atuin, git, lazygit, jq, gh, terraform, fluxcd, kustomize, sops, age, k9s, nodejs, go, cargo, uv, hugo, pandoc, and more (see `home/common.nix`).

**Shell** — Zsh with [Antidote](https://getantidote.github.io/) plugins, vi-mode, fzf-tab, autosuggestions, fast-syntax-highlighting, and [Starship](https://starship.rs/) prompt.

**Dotfile symlinks** — config files for git, kitty, neovim, vim, starship, direnv, emacs, LaTeX, and others are symlinked into place by home-manager.

**macOS system defaults** — Dock, Finder, trackpad, keyboard, Touch ID sudo, and Homebrew casks (via nix-darwin).

## Repository layout

| Path | Description |
| ---- | ----------- |
| `flake.nix` | Nix flake — inputs and host outputs |
| `home/common.nix` | Shared home-manager config (packages, shell, programs) |
| `home/darwin.nix` | macOS-specific home-manager settings |
| `home/nixos.nix` | NixOS-specific home-manager settings |
| `modules/darwin.nix` | nix-darwin system config (Homebrew casks, macOS defaults) |
| `modules/nixos.nix` | NixOS system config |
| `hosts/` | Per-host overrides |
| `nvim/` | Neovim config based on [LazyVim](https://www.lazyvim.org/) |
| `git/` | Git aliases, delta pager, SSH signing, kitty difftool |
| `kitty/` | Kitty terminal config |
| `emacs/` | Doom Emacs config |
| `bootstrap.sh` | One-time setup (Nix, rustup, ghcup, SSH signing keys) |
| `justfile` | Common tasks (`just switch`, `just update`, ...) |

## Installation

```sh
git clone git://github.com/waveFrontSet/dotfiles.git
cd dotfiles
./bootstrap.sh   # install Nix, rustup, ghcup, configure SSH signing
just switch       # apply the nix configuration
```

## Day-to-day usage

```sh
just switch    # rebuild and activate the current configuration
just update    # update flake inputs and rebuild
just hm-switch # standalone home-manager switch (no nix-darwin/NixOS)
```
