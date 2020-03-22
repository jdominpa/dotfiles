#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Font Tools\n\n"

brew_install "Source code pro font" "font-source-code-pro" "homebrew/cask-fonts" "cask"
