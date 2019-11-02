#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Font Tools\n\n"

brew_install "Dejavu sans mono nerd font" "font-dejavusansmono-nerd-font-mono" "homebrew/cask" "cask"
