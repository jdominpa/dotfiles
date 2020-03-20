#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

brew_install "Neovim" "neovim"
brew_install "Emacs" "emacs-plus" "d12frosted/emacs-plus"
execute "ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app" \
    "Setting emacs.app sym link"

execute "git clone https://github.com/hlissner/doom-emacs ~/.emacs.d" \
    "Cloning Doom-Emacs"
printf "\n" &&
~/.emacs.d/bin/doom install &&
~/.emacs.d/bin/doom refresh
