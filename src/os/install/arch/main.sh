#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

./git.sh

update_and_upgrade
install_yay

./desktop.sh
./fonts.sh
./terminal.sh
./gui_progs.sh
./cmd_ln_tools.sh
./editors.sh
./proglang.sh
./theming.sh

./cleanup.sh
