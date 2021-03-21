#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Keyboard\n\n"

execute "gsettings set org.gnome.settings-daemon.plugins.power power-button-action 'suspend'" \
    "Power button suspends system"

execute "gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type 'suspend'" \
    "Power button suspends system"

execute "gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 2700" \
    "Power button suspends system"
