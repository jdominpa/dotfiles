#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Command Line Tools\n\n"

install_package "cURL" "curl"
install_package "ripgrep" "ripgrep"
install_package "SSH" "openssh"
install_package "unrar" "unrar"
install_package "unzip" "unzip"

# Clipboard utility
install_package "xclip" "xclip"

# Pacman utilities
install_package "Pacman utilities" "pacman-contrib"
