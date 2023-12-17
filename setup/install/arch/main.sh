#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

./git.sh

update_and_upgrade
install_yay

./stow.sh

./desktop.sh
./fonts.sh
./cmd_ln_tools.sh
./terminal.sh
./gui_progs.sh
./editors.sh
./proglang.sh
./theming.sh

./cleanup.sh
