#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Theming\n\n"

install_package "Gnome tweaks" "gnome-tweaks"
install_package "Gnome tweak tool" "gnome-tweak-tool"
