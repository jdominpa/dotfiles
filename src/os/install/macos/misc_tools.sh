#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Miscellaneous Tools\n\n"

brew_install "ShellCheck" "shellcheck"
brew_install "Fzf" "fzf"
brew_install "Node.js" "node"
brew_install "C language server for vim" "ccls"

brew_install "Python 3" "python"
execute "python3 -m pip install --user --upgrade pynvim" \
    "Pynvim module for neovim"

brew_install "Pandoc" "pandoc"
