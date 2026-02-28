# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

# ── Vi mode (replaces zprezto: editor) ──────────────────────────────────────

bindkey -v
export KEYTIMEOUT=1
autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# ── Completion system ───────────────────────────────────────────────────────

# Add custom functions
fpath+=~/.zfunc
for func in ~/.zfunc/*(.N); do
  autoload -Uz "${func:t}"
done

# ── Antidote (plugin manager) ──────────────────────────────────────────────

source ${BREW_PREFIX}/opt/antidote/share/antidote/antidote.zsh
antidote load ${ZDOTDIR:-$HOME}/.zsh_plugins.txt

# ── Syntax highlighting config ──────────────────────────────────────────────

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern line cursor root)

# ── Autosuggestion config ───────────────────────────────────────────────────

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'
bindkey '^ ' autosuggest-accept

# ── Tool initialization ────────────────────────────────────────────────────

# fzf keybindings and completion
source <(fzf --zsh)
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || bat --style numbers,changes --color=always {} || tree -C {}) 2> /dev/null | head -200'"

eval "$(zoxide init zsh)"
eval "$(atuin init zsh --disable-up-arrow)"
eval "$(direnv hook zsh)"


# ── Aliases ─────────────────────────────────────────────────────────────────

# Pull everything I need from my reps
alias gp="cd ~/dotfiles; git p; pc rep/bit; cd ~/org; git p; cd ~/latex-docs; git p; cd ~"

# Emacs
alias em="open /Applications/Emacs.app"

# Jump to folder (zoxide)
alias j="z"

# Modern ls replacement (eza)
alias ls="eza --icons"
alias ll="eza -la --icons --git"
alias la="ll -A"
alias tree="eza --tree --icons"

# Kitty shortcuts
alias icat="kitty +kitten icat"
alias ssh="kitty +kitten ssh"

# Use ripgrep instead of grep
alias grep=rg

# On M1 provide fallback alternative ibrew
alias brew=${BREW_PREFIX}/bin/brew
alias ibrew="arch -x86_64 /usr/local/bin/brew"

# ── Python / direnv ────────────────────────────────────────────────────────

# Standard setup for new python project
,envrc_and_allow() {
  echo "layout uv" > .envrc
  direnv allow
}

# ── Prompt ──────────────────────────────────────────────────────────────────

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# ── Runtime environments ───────────────────────────────────────────────────
. "$HOME/.cargo/env"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/paul/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/paul/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/paul/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/paul/google-cloud-sdk/completion.zsh.inc'; fi
