# ── Zsh options (replaces zprezto: environment, directory, history) ──────────

# Directory navigation
setopt AUTO_CD              # cd by typing directory name
setopt AUTO_PUSHD           # push old directory onto stack
setopt PUSHD_IGNORE_DUPS    # don't push duplicates
setopt PUSHD_SILENT         # don't print directory stack
setopt PUSHD_TO_HOME        # pushd with no args goes to ~
setopt CDABLE_VARS          # try expanding as variable before directory
setopt EXTENDED_GLOB        # extended globbing (#, ~, ^)

# History
HISTFILE="${ZDOTDIR:-$HOME}/.zsh_history"
HISTSIZE=50000
SAVEHIST=50000
setopt BANG_HIST             # treat ! specially in expansion
setopt EXTENDED_HISTORY      # write timestamps to history
setopt SHARE_HISTORY         # share history between sessions
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE     # don't record commands starting with space
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY           # show expanded history before executing
setopt HIST_BEEP

# General
setopt INTERACTIVE_COMMENTS  # allow comments in interactive shells
setopt COMBINING_CHARS       # handle combining characters correctly
setopt NO_MAIL_WARNING
setopt NO_BEEP

# ── Completion system ───────────────────────────────────────────────────────

# Add custom functions
fpath+=~/.zfunc
for func in ~/.zfunc/*(.N); do
  autoload -Uz "${func:t}"
done

# ── Antidote (plugin manager) ──────────────────────────────────────────────

# Antidote: Homebrew on macOS, git clone fallback on Linux (e.g., devcontainers)
if [[ -f ${BREW_PREFIX:-/opt/homebrew}/opt/antidote/share/antidote/antidote.zsh ]]; then
  source ${BREW_PREFIX}/opt/antidote/share/antidote/antidote.zsh
elif [[ -f ${HOME}/.antidote/antidote.zsh ]]; then
  source ${HOME}/.antidote/antidote.zsh
fi
antidote load ${ZDOTDIR:-$HOME}/.zsh_plugins.txt

# ── Syntax highlighting config ──────────────────────────────────────────────

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern line cursor root)

# ── Autosuggestion config ───────────────────────────────────────────────────

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'

# Apply custom keybindings after zsh-vi-mode finishes its deferred init,
# otherwise zvm overwrites them when the first prompt renders.
zvm_after_init_commands+=(
  "bindkey '^ ' autosuggest-accept"
  "bindkey '^r' atuin-search"
)

# ── Tool initialization ────────────────────────────────────────────────────

# fzf keybindings and completion
if (( $+commands[fzf] )); then
  source <(fzf --zsh)
  export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || bat --style numbers,changes --color=always {} || tree -C {}) 2> /dev/null | head -200'"
fi

(( $+commands[zoxide] ))  && eval "$(zoxide init zsh)"
(( $+commands[atuin] ))   && eval "$(atuin init zsh --disable-up-arrow)"
(( $+commands[direnv] ))  && eval "$(direnv hook zsh)"

# ── Aliases ─────────────────────────────────────────────────────────────────

# Jump to folder (zoxide)
alias j="z"

# Modern ls replacement (eza)
if (( $+commands[eza] )); then
  export EZA_COLORS="xx=36"  # Fix punctuation invisible on solarized dark
  alias ls="eza --icons"
  alias ll="eza -la --icons --git"
  alias la="ll -A"
  alias tree="eza --tree --icons"
fi

# Kitty shortcuts (only when running in Kitty terminal)
if (( $+commands[kitty] )); then
  alias icat="kitty +kitten icat"
  alias ssh="kitty +kitten ssh"
fi

# Use ripgrep instead of grep
(( $+commands[rg] )) && alias grep=rg

# Homebrew aliases (macOS only)
if [[ "$OSTYPE" == darwin* ]]; then
  alias brew=${BREW_PREFIX}/bin/brew
  # On M1 provide fallback alternative ibrew for Intel-only packages
  [[ $(uname -m) == "arm64" ]] && alias ibrew="arch -x86_64 /usr/local/bin/brew"
fi

alias gcal="gcalcli"

# ── Python / direnv ────────────────────────────────────────────────────────

# Standard setup for new python project
,envrc_and_allow() {
  echo "layout uv" > .envrc
  direnv allow
}

# ── Prompt ──────────────────────────────────────────────────────────────────

# Starship
eval "$(starship init zsh)"

# ── Runtime environments ───────────────────────────────────────────────────

[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env" # ghcup-env

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Google Cloud SDK (macOS paths)
if [[ "$OSTYPE" == darwin* ]]; then
  if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then . "$HOME/google-cloud-sdk/path.zsh.inc"; fi
  if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then . "$HOME/google-cloud-sdk/completion.zsh.inc"; fi
fi
