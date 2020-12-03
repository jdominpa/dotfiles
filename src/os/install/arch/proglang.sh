#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Programming Languages\n\n"

# Python 3
install_package "Python 3" "python"

# Haskell
install_package "GHC" "ghc"
install_package "Cabal" "cabal-install"
install_package "Stack" "stack"
