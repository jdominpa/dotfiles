#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Miscellaneous Tools\n\n"

# cURL utility
install_package "cURL" "curl"

# Process monitor
install_package "htop" "htop"

# Check shell syntax
install_package "ShellCheck" "shellcheck"

# Unrar and unzip programs
install_package "unrar" "unrar"
install_package "unzip" "unzip"

# Modify key behavior
install_package "xcape" "xcape"

# Clipboard utility
install_package "xclip" "xclip"

# Pacman utilities
install_package "Reflector" "reflector"
install_package "Pacman utilities" "pacman-contrib"
