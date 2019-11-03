#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Font Tools\n\n"

brew_install "Source code pro nerd font" "font-sourcecodepro-nerd-font-mono" "homebrew/cask-fonts" "cask"
