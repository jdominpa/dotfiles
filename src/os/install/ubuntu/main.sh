#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

update
upgrade

./git.sh
./zsh.sh
./misc.sh
./misc_tools.sh
./editors.sh
./theming.sh

./cleanup.sh
