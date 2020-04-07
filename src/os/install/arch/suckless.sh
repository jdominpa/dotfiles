#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Suckless programs\n\n"

initialDir="$(pwd)"

# Install dmenu
execute "git clone https://git.suckless.org/dmenu ~/Programs/dmenu" \
    "Cloning dmenu"
execute "cd ~/Programs/dmenu && bash -c $HOME/.local/bin/suckless/suckinstall && cd '$initialDir'" \
    "Installing dmenu"
