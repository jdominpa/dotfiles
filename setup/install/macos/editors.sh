#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Text editors\n\n"

brew_install "Neovim" "neovim"

brew_install "Emacs" "emacs-plus@28" "d12frosted/emacs-plus"
# brew_install "Emacs" "emacs" "homebrew/cask" "--cask"
