#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

install_package "Neovim" "neovim"

install_package "Emacs" "emacs"
install_package "Emacs dependencies" "ripgrep fd-find clang"
execute "git clone https://github.com/hlissner/doom-emacs ~/.emacs.d" \
    "Cloning Doom-Emacs"
printf "\n" && ~/.emacs.d/bin/doom install && ~/.emacs.d/bin/doom refresh