#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

install_package "Neovim" "neovim"

install_package "Emacs" "emacs"
install_package "ripgrep" "ripgrep"
