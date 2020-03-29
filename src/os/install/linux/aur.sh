#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   AUR programs\n\n"

# Status bar
install_aur_package "Polybar" "polybar"
install_aur_package "Brave" "brave-bin"
install_aur_package "Ant Dracula gtk theme" "ant-dracula-gtk-theme"
