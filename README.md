# Dotfiles

Personal configuration files managed with [Dotbot](https://github.com/anishathalye/dotbot).

## What's included

| Directory      | Description                                              |
| -------------- | -------------------------------------------------------- |
| `zsh/`         | Zsh config with [Antidote](https://getantidote.github.io/) plugin manager |
| `nvim/`        | Neovim config based on [LazyVim](https://www.lazyvim.org/) |
| `vim/`         | Vim config                                               |
| `git/`         | Git aliases, delta pager, GPG signing, kitty difftool    |
| `kitty/`       | Kitty terminal config                                    |
| `tmux/`        | Tmux config                                              |
| `brew/`        | Homebrew Brewfile (bat, eza, fd, ripgrep, fzf, zoxide, atuin, ...) |
| `direnv/`      | direnv config                                            |
| `gnupg/`       | GPG agent config (pinentry-mac)                          |
| `emacs/`       | Doom Emacs config                                        |
| `latex/`       | LaTeX packages                                           |
| `latexmk/`     | latexmk config                                           |
| `mutt/`        | Mutt email client config                                 |
| `osx/`         | macOS defaults                                           |

## Installation

```sh
git clone git://github.com/waveFrontSet/dotfiles.git
cd dotfiles
./install
```

This will:

1. Symlink all config files to their expected locations
2. Install Homebrew (if missing) and packages from the Brewfile
3. Install [uv](https://github.com/astral-sh/uv) and [Rustup](https://rustup.rs/)
