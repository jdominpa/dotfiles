#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Theming\n\n"

install_package "Gnome tweaks" "gnome-tweaks"
install_package "Gnome tweak tool" "gnome-tweak-tool"

execute "mkdir -p ~/.themes/dracula && git clone https://github.com/EliverLara/Ant-Dracula.git ~/.themes/dracula" \
    "Install dracula gtk theme"
