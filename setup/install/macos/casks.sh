#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Cask Applications\n\n"

#brew_install "iTerm2" "iterm2" "homebrew/cask" "--cask"
brew_install "Whatsapp" "whatsapp" "homebrew/cask" "--cask"
brew_install "Discord" "discord" "homebrew/cask" "--cask"
