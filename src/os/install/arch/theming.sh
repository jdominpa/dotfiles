#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Theming packages\n\n"

install_package "Papirus icon theme" "papirus-icon-theme"
install_package "Arc gtk theme" "arc-gtk-theme"
