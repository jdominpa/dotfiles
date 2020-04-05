#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   AUR programs\n\n"

# AUR packages
install_aur_package "Nerd fonts" "nerd-fonts-complete"
install_aur_package "Ant gtk theme" "ant-gtk-theme"
install_aur_package "Paper icon theme" "paper-icon-theme"
