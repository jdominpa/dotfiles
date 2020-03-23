#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Neovim
install_package "Neovim" "neovim"
install_package "Python 3" "python"
install_package "Pip" "python-pip"
execute "python3 -m pip install --user --upgrade pynvim" \
    "Pynvim module for neovim"

# Emacs followed by doom-emacs and it's dependencies
install_package "Emacs" "emacs"
install_package "ripgrep" "ripgrep"
install_package "clang" "clang"
install_package "tar" "tar"
install_package "fd" "fd"

execute "git clone https://github.com/hlissner/doom-emacs ~/.emacs.d" \
    "Cloning Doom-Emacs"
printf "\n" && ~/.emacs.d/bin/doom install && ~/.emacs.d/bin/doom refresh
