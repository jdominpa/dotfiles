#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Miscellaneous\n\n"

brew_install "Spotify" "spotify" "homebrew/cask" "cask"
brew_install "Whatsapp" "whatsapp" "homebrew/cask" "cask"
brew_install "Bitwarden" "bitwarden" "homebrew/cask" "cask"
