#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

update
upgrade

./stow.sh

./git.sh
./cmd_ln_tools.sh
./zsh.sh
./editors.sh
./fonts.sh

./cleanup.sh
