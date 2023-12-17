#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Zsh\n\n"

execute "mkdir -p $HOME/.config/zsh" \
  "Creating zsh config directory"

execute "git clone https://github.com/zsh-users/zsh-autosuggestions.git $HOME/.config/zsh/zsh-autosuggestions" \
  "Cloning zsh-autosuggestions repository"
