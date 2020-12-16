#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Command Line Tools\n\n"

install_package "Ripgrep" "ripgrep"
install_package "cURL" "curl"
install_package "ShellCheck" "shellcheck"
install_package "Unzip" "unzip"
