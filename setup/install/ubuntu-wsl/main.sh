#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

update
upgrade

./stow.sh

./git.sh
./zsh.sh
./cmd_ln_tools.sh
./editors.sh
./fonts.sh

./cleanup.sh
