# Dotfiles

[![AI-DECLARATION: pair](https://img.shields.io/badge/䷼%20AI--DECLARATION-pair-ffedd5?labelColor=ffedd5)](https://ai-declaration.md)

Personal configuration files managed with [Nix](https://nixos.org/),
[home-manager](https://github.com/nix-community/home-manager), and
[nix-darwin](https://github.com/LnL7/nix-darwin).

## Supported hosts

| Host | OS | Flake output |
| ---- | -- | ------------ |
| `work-mbp` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `mini` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `no-mans-land` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `home-laptop` | NixOS (x86_64-linux) | `nixosConfigurations` |

## What's managed

**`home/`** — Home-manager modules for packages, shell (Zsh, Starship), git, kitty, direnv, and other user-level dotfiles. Shared config lives in `common.nix`; OS-specific overrides in `darwin.nix` and `nixos.nix`.

**`modules/`** — OS-level system configurations. `darwin.nix` handles nix-darwin settings (Homebrew casks, macOS defaults like Dock, Finder, trackpad, Touch ID sudo). `nixos.nix` handles NixOS system config.

**`hosts/`** — Per-host overrides layered on top of the shared modules.

**`nvim/`, `vim/`** — Editor configurations (Neovim based on [LazyVim](https://www.lazyvim.org/), Vim). Currently top-level, may move into `home/` in the future.

## Repository layout

```
.
├── flake.nix                  # Nix flake — inputs and host outputs
├── home/                      # Home-manager modules (dotfiles & programs)
│   ├── common.nix             #   Shared config (packages, shell, programs)
│   ├── darwin.nix             #   macOS-specific settings
│   ├── nixos.nix              #   NixOS-specific settings
│   ├── zsh.nix                #   Zsh configuration
│   ├── git.nix                #   Git configuration
│   ├── kitty.nix              #   Kitty terminal configuration
│   ├── direnv.nix             #   Direnv configuration
│   └── agents/                #   AI agent configurations
├── modules/                   # OS-level system configurations
│   ├── darwin.nix             #   nix-darwin (Homebrew casks, macOS defaults)
│   └── nixos.nix              #   NixOS system config
├── hosts/                     # Per-host overrides
├── nvim/                      # Neovim config (LazyVim)
├── vim/                       # Vim config
├── bootstrap.sh               # One-time setup (Nix, rustup, ghcup, SSH keys)
└── justfile                   # Common tasks (just switch, just update, ...)
```

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
just bootstrap  # first-time nix-darwin bootstrap
```
