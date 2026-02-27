# List available recipes
default:
    @just --list

# Run full install (link dotfiles, install packages, setup toolchains)
install: link brew

# Link dotfiles via dotbot
link:
    ./install

# Install packages from Brewfile (idempotent)
brew:
    brew bundle --file=brew/Brewfile

# Update brew packages and neovim plugins
update:
    brew bundle --file=brew/Brewfile
    nvim --headless "+Lazy! sync" +qa

# Apply macOS system defaults
[macos]
macos:
    ./osx/defaults.sh
