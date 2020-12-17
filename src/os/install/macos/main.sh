#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

./xcode.sh
./homebrew.sh
./zsh.sh
#./bash.sh

./git.sh
./gpg.sh
./casks.sh
./cmd_ln_tools.sh
./editors.sh
./font_tools.sh
