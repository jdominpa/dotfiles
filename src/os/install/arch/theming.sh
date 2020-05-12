#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Themeing packages\n\n"

install_package "Papirus icon theme" "papirus-icon-theme"
install_aur_package "Ant gtk theme" "ant-gtk-theme"
