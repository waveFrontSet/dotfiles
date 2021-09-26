# My dotfiles

This repository contains the configuration files for my shell, my text editors and some other tools I use frequently.

## Installation

``` sh
git clone git://github.com/waveFrontSet/dotfiles.git
```

Symlink everything using the bootstrap script

``` sh
./bootstrap
```

OR use stow to pick which config files you need, for example

``` sh
stow vim zsh
```

only creates symlinks for the config files of vim and zsh.

Let NeoBundle install vim plugins
    
``` sh
    git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim
    vim -c NeoBundleInstall
```
