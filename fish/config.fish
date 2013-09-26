function fish_prompt
    set_color red 
    echo -n '['
    set_color normal
    echo -n (date "+%H:%M")
    set_color red
    echo -n '] '
    set_color normal
    echo -n (whoami)
    echo -n '@'
    set_color red
    echo -n (hostname|cut -d . -f 1)
    echo -n ' '
    set_color red
    echo -n (prompt_pwd)
    echo -n '$ '
end

function ej
    sudo diskutil eject $argv
end

function sk
    open -a Skim $argv
end

function v
    vim $argv
end

function gv
    mvim $argv
end

function fi
    open -a Finder .
end

set -x EDITOR vim
set -x PAGER vimpager 
set -x MOBDOC ~/Library/Mobile\ Documents/com~apple~Preview/Documents
set -x TEXPATH ~/Library/texmf/tex/latex/misc
set -x PATH /usr/local/opt/coreutils/libexec/gnubin /usr/local/bin /usr/local/texlive/2013/bin/x86_64-darwin /Users/paul/Library/Python/2.7/bin /usr/local/opt/ruby/bin $PATH
set -x MANPATH /usr/local/opt/coreutils/libexec/gnuman /usr/share/man /usr/local/share/man /usr/X11/share/man /usr/local/texlive/2013/texmf/doc/man $MANPATH
