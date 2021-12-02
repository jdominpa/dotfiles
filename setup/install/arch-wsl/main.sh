#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

./git.sh

update_and_upgrade
install_package "Base-devel" "base-devel"
install_yay

./stow.sh

./terminal.sh
./cmd_ln_tools.sh
./editors.sh
./proglang.sh

./cleanup.sh
