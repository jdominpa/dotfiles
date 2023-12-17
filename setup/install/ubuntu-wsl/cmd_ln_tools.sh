#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Command Line Tools\n\n"

install_package "Build essentials like gcc" "build-essential"
install_package "Man pages for dev tools" "manpages-dev"
install_package "Ripgrep" "ripgrep"
install_package "fzf" "fzf"
install_package "cURL" "curl"
install_package "Unzip" "unzip"
