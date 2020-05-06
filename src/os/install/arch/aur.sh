#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   AUR programs\n\n"

install_aur_package "Roboto Nerd font" "nerd-fonts-roboto-mono"
install_aur_package "Ant gtk theme" "ant-gtk-theme"
install_aur_package "Macos icon theme" "mcmojave-circle-icon-theme"
