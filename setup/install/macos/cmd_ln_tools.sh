#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Command Line Tools\n\n"

brew_install "Coreutils" "coreutils"
brew_install "Python 3" "python"
brew_install "Ripgrep" "ripgrep"
brew_install "fzf" "fzf"
brew_install "Stow" "stow"
