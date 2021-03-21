#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Command Line Tools\n\n"

install_package "cURL" "curl"
install_package "ripgrep" "ripgrep"
install_package "SSH" "openssh"
install_package "Stow" "stow"
install_package "unrar" "unrar"
install_package "unzip" "unzip"

# Process monitor
install_package "htop" "htop"

# Modify key behavior
install_package "xcape" "xcape"

# Clipboard utility
install_package "xclip" "xclip"

# Pacman utilities
install_package "Reflector" "reflector"
install_package "Pacman utilities" "pacman-contrib"
