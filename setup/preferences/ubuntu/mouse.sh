#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Keyboard\n\n"

execute "gsettings set org.gnome.desktop.peripherals.mouse accel-profile 'flat'" \
    "Disable mouse acceleration"

execute "gsettings set org.gnome.desktop.peripherals.mouse speed 0.0" \
    "Lower mouse speed"
