# Dotfiles

[![AI-DECLARATION: pair](https://img.shields.io/badge/䷼%20AI--DECLARATION-pair-ffedd5?labelColor=ffedd5)](https://ai-declaration.md)

Personal configuration files managed with [Nix](https://nixos.org/),
[home-manager](https://github.com/nix-community/home-manager), and
[nix-darwin](https://github.com/LnL7/nix-darwin).

> **Note:** This repo is heavily personalized — usernames, hostnames, Dock
> apps, Homebrew casks, and SSH key names are all mine. It is not a drop-in
> config for other users without editing those values.

## Supported hosts

| Host           | OS                     | Flake output                                       |
| -------------- | ---------------------- | -------------------------------------------------- |
| `no-mans-work` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `no-mans-mini` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `no-mans-land` | macOS (aarch64-darwin) | `darwinConfigurations` — nix-darwin + home-manager |
| `home-laptop`  | NixOS (x86_64-linux)   | `nixosConfigurations`                              |

## Repository layout

```text
.
├── flake.nix                  # Nix flake — inputs and host outputs
├── home/                      # Home-manager modules (dotfiles & programs)
│   ├── common.nix             #   Shared config (packages, session vars)
│   ├── darwin.nix             #   macOS-specific settings
│   ├── nixos.nix              #   NixOS-specific settings
│   ├── zsh.nix                #   Zsh, Starship, fzf, zoxide, atuin
│   ├── git.nix                #   Git configuration
│   ├── direnv.nix             #   Direnv (incl. nix-direnv)
│   ├── gh.nix                 #   GitHub CLI + gh-dash
│   ├── kitty/                 #   Kitty terminal (config + kittens)
│   ├── nvim/                  #   Neovim config (LazyVim, nix-pinned plugins)
│   ├── vim/                   #   Vim config
│   └── agents/                #   AI agent configurations
├── modules/                   # OS-level system configurations
│   ├── darwin.nix             #   nix-darwin (Homebrew casks, macOS defaults)
│   └── nixos.nix              #   NixOS system config
├── hosts/                     # Per-host overrides
├── overlays/                  # Package pins / custom derivations
├── bootstrap.sh               # One-time setup (Nix, rustup, ghcup, SSH keys)
└── justfile                   # Common tasks (just switch, just update, ...)
```

## Prerequisites

- Git
- macOS: [Determinate Nix](https://determinate.systems/), configured through
  its nix-darwin module; `bootstrap.sh` installs it if missing
- NixOS: the system Nix installation
- `just` — available after the first Nix activation (bootstrap uses raw `nix run`)
- macOS only: Homebrew casks are managed *through* nix-darwin; Homebrew itself
  must be installed once manually
- An SSH key at `~/.ssh/id_ed25519` (`.pub` used for commit signing)

## Installation

The repo **must** be cloned to `~/dotfiles` — the Neovim config is an
out-of-store symlink pointing there (`home/common.nix`).

```sh
git clone git@github.com:waveFrontSet/dotfiles.git ~/dotfiles
cd ~/dotfiles
./bootstrap.sh
```

`bootstrap.sh` installs Nix, rustup, and ghcup if missing, creates
`~/.ssh/allowed_signers`, and reminds you to add the SSH signing key to GitHub.

Then, on macOS:

```sh
just bootstrap      # first nix-darwin activation (darwin-rebuild not yet installed)
just install-hooks  # install git pre-commit hooks (prek)
```

On NixOS:

```sh
sudo nixos-rebuild switch --flake ~/dotfiles#home-laptop
```

The macOS configurations use Determinate Nix and declare custom Nix settings
through its nix-darwin module. `bootstrap.sh` installs Determinate Nix only on
machines without Nix; use the [Determinate macOS installer](https://install.determinate.systems/determinate-pkg/stable/Universal)
to migrate an existing upstream installation. NixOS uses upstream Nix with
`nix-command` and `flakes` enabled in `modules/nixos.nix`.

> **Warning:** `modules/darwin.nix` sets `homebrew.onActivation.cleanup = "zap"`.
> Any Homebrew cask or formula installed on the machine but *not* listed in the
> config is **uninstalled (zapped, including app data) on activation**. Add
> existing casks to the list before the first switch.

## Day-to-day usage

```sh
just switch    # rebuild and activate the current configuration
just update    # update flake inputs and rebuild (commit flake.lock afterwards)
just gc        # drop system generations older than 30d, GC + optimise the store
nix fmt        # format all nix files (nixfmt)
```

## Where to change things

- Packages shared everywhere: `home/common.nix`
- macOS-only user packages/settings: `home/darwin.nix`
- NixOS-only user settings: `home/nixos.nix`
- macOS defaults, Homebrew casks, Touch ID sudo: `modules/darwin.nix`
- NixOS system config: `modules/nixos.nix`
- Per-machine overrides: `hosts/*.nix`
- Zsh/Git/Kitty/Direnv/gh: `home/*.nix`, `home/kitty/`
- Neovim/Vim: `home/nvim/`, `home/vim/`
- AI agent configs: `home/agents/`
- Version pins / custom packages: `overlays/`

## Adding a new host

1. Create `hosts/<platform>-<name>.nix` with the host-specific overrides.
2. Register it in `flake.nix` (`darwinConfigurations` or `nixosConfigurations`),
   picking username and architecture.
3. Run `just switch` (or the explicit `darwin-rebuild`/`nixos-rebuild` command).

## Troubleshooting

- `darwin-rebuild: command not found` — first activation; run `just bootstrap`.
- Neovim config is writable on purpose: `~/.config/nvim` is an out-of-store
  symlink to `~/dotfiles/home/nvim` so `lazyvim.json` stays editable.
- Commit signing errors — check `~/.ssh/allowed_signers` exists and the public
  key is registered as a *signing* key on GitHub.
