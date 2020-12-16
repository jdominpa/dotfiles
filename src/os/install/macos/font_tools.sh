#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Font Tools\n\n"

brew_install "Fira Code font" "font-fira-code" "homebrew/cask-fonts" "--cask"
