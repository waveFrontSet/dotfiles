# Dotfiles

[![AI-DECLARATION: pair](https://img.shields.io/badge/䷼%20AI--DECLARATION-pair-ffedd5?labelColor=ffedd5)](https://ai-declaration.md)

Personal configuration files managed with [Nix](https://nixos.org/),
[home-manager](https://github.com/nix-community/home-manager), and
[nix-darwin](https://github.com/LnL7/nix-darwin).

> **Note:** This repo is heavily personalized — usernames, hostnames, Dock apps,
> Homebrew casks, and SSH key names are all mine. It is not a drop-in config for
> other users without editing those values.

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
├── templates/                 # devenv project templates (rust, haskell, python)
├── devenv.nix                 # devenv tasks (devr switch, devr update, devr gc, ...)
├── devenv.yaml                # devenv input pins
├── bootstrap.sh               # One-time setup (Nix, SSH keys)
└── README.md
```

## Prerequisites

- Git
- macOS: [Determinate Nix](https://determinate.systems/), configured through its
  nix-darwin module; `bootstrap.sh` installs it if missing
- NixOS: the system Nix installation
- devenv — installed via `programs.devenv` (with zsh auto-activation); bootstrap
  runs before the first activation, so it invokes devenv through `nix shell`
- macOS only: Homebrew casks are managed _through_ nix-darwin; Homebrew itself
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

`bootstrap.sh` installs Nix if missing, creates `~/.ssh/allowed_signers`, and
reminds you to add the SSH signing key to GitHub.

Then, on macOS:

```sh
nix shell nixpkgs#devenv -c devenv tasks run bootstrap  # first nix-darwin activation
                                                        # (darwin-rebuild not yet installed)
```

On NixOS:

```sh
sudo nixos-rebuild switch --flake ~/dotfiles#home-laptop
```

After the first activation, devenv is available globally: trust the repo once
with `devenv allow` (from inside `~/dotfiles`), and the environment — including
the tasks — auto-activates when you `cd` into it. Git hooks (nixfmt, statix,
markdownlint) are installed by devenv on shell entry.

The macOS configurations use Determinate Nix and declare custom Nix settings
through its nix-darwin module. `bootstrap.sh` installs Determinate Nix only on
machines without Nix; use the
[Determinate macOS installer](https://install.determinate.systems/determinate-pkg/stable/Universal)
to migrate an existing upstream installation. NixOS uses upstream Nix with
`nix-command` and `flakes` enabled in `modules/nixos.nix`.

> **Warning:** `modules/darwin.nix` sets
> `homebrew.onActivation.cleanup = "zap"`. Any Homebrew cask or formula
> installed on the machine but _not_ listed in the config is **uninstalled
> (zapped, including app data) on activation**. Add existing casks to the list
> before the first switch.

## Day-to-day usage

```sh
devr switch    # rebuild and activate the current configuration
devr update    # update flake inputs and rebuild (commit flake.lock afterwards)
devr gc        # drop system generations older than 30d, GC + optimise the store
nix fmt        # format all nix files (nixfmt)
```

`devr` is an alias for `devenv tasks run`; the tasks are defined in
`devenv.nix` and operate on the repo root regardless of the current directory
inside `~/dotfiles`. devenv requires namespaced task names, so each task lives
in its own single-task namespace (`bootstrap:main`, `switch:main`, …) — running
the bare namespace (`devr switch`) therefore runs exactly that task.

## devenv project templates

Language toolchains are not installed globally — each project declares its own
environment with [devenv](https://devenv.sh). Scaffold a new project with:

```sh
devi-rust            # Rust: stable toolchain, rustfmt + clippy commit hooks
devi-hask            # Haskell: GHC, Cabal, HLS; global (overlay-pinned) fourmolu
devi-py              # Python: uv sync, ruff + ruff-format commit hooks
devi-rust my-proj    # same, into a (new) subdirectory
```

Each template ships a `devenv.nix` and `devenv.yaml`. The `devi-*` helpers fetch
the `.gitignore` from the pinned
[github/gitignore](https://github.com/github/gitignore) flake input at
activation time and merge it with the devenv entries into a normal static
project file when scaffolding. Run `devenv shell` inside the project to enter
the environment (it also installs the git commit hooks); `devenv up` starts
processes. `devr` is an alias for `devenv tasks run`.

## Removing rustup and ghcup (one-time)

If a machine still carries the old globally-installed toolchains:

```sh
rustup self uninstall   # removes ~/.rustup and ~/.cargo (toolchains + cargo-installed binaries)
ghcup nuke              # removes ~/.ghcup incl. GHC, HLS and ghcup itself
rm -rf ~/.cabal         # optional: cabal-installed global packages / build cache
```

Then confirm nothing is left:

```sh
command -v rustup ghcup ghc cabal cargo
```

(No output means clean.) The `~/.cargo/bin` and `~/.ghcup/bin` PATH entries have
already been dropped from `home/common.nix`.

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
3. Run `devr switch` (or the explicit `darwin-rebuild`/`nixos-rebuild` command).

## Troubleshooting

- `darwin-rebuild: command not found` — first activation; run `nix shell
  nixpkgs#devenv -c devenv tasks run bootstrap` from `~/dotfiles`.
- Neovim config is writable on purpose: `~/.config/nvim` is an out-of-store
  symlink to `~/dotfiles/home/nvim` so `lazyvim.json` stays editable.
- Commit signing errors — check `~/.ssh/allowed_signers` exists and the public
  key is registered as a _signing_ key on GitHub.
- `bw` asks for the master password — the Desktop app is unreachable: check it
  is running and `BWBIO_VERBOSE=true bwbio unlock` shows a Touch ID prompt.
