#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi
#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export EDITOR='nvim'
export VISUAL='nvim'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi
export DOCKER_DEFAULT_PLATFORM=linux/amd64

#
# Paths
#

typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  ~/.poetry/bin
  /Library/TeX/texbin
  /opt/homebrew/opt/coreutils/libexec/gnubin
  /opt/homebrew/bin
  /opt/homebrew/sbin
  ~/.emacs.d/bin
  /usr/local/opt/coreutils/libexec/gnubin
  /usr/local/{bin,sbin}
  /Users/paul/anaconda/bin
  /usr/local/opt/ruby/bin
  ~/.local/bin
  $path
)

# Brew prefix
# Check for M1 Arch
export BREW_PREFIX="/usr/local"
if [[ $(uname -m) == "arm64" ]]; then
    export BREW_PREFIX="/opt/homebrew"
fi

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

if [ -e /Users/paul/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/paul/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
. "$HOME/.cargo/env"
