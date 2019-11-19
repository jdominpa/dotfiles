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
./misc.sh
./misc_tools.sh
./neovim.sh
./font_tools.sh
