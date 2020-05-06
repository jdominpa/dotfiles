#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Text editors\n\n"

install_package "Neovim" "neovim"

install_package "Emacs" "emacs"
install_package "Emacs dependencies" "ripgrep clang"

#install_package "Doom-emacs dependencies" "fd-find clang"
#execute "git clone https://github.com/hlissner/doom-emacs ~/.emacs.d" \
#    "Cloning Doom-Emacs"
#printf "\n" && ~/.emacs.d/bin/doom install && ~/.emacs.d/bin/doom refresh
