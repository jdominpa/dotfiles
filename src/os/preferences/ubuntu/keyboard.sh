#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Keyboard\n\n"

execute "gsettings set org.gnome.desktop.peripherals.keyboard delay 150" \
    "Faster initial repeat delay"

execute "gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15" \
    "Faster repeat rate"

execute "gsettings set org.gnome.desktop.input-sources xkb-options \"['caps:escape']\"" \
    "Make caps lock behave as escape key"
