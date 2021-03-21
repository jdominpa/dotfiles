#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Display settings\n\n"

execute "gsettings set org.gnome.settings-daemon.plugins.color night-light-enabled true" \
        "Activate night shift"

execute "gsettings set org.gnome.shell.extensions.dash-to-dock dash-max-icon-size 34" \
        "Reduce dock's icon size"

execute "gsettings set org.gnome.shell.extensions.dash-to-dock dock-fixed false" \
        "Hide dock automatically"
