#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

./git.sh

update_and_upgrade
install_yay

./desktop.sh
./terminal.sh
./browsers.sh
./misc.sh
./misc_tools.sh
./editors.sh
./aur.sh

./cleanup.sh
