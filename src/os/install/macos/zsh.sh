#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Zsh\n\n"

execute "mkdir -p $HOME/.zsh" \
	"Creating .zsh directory"

execute "git clone https://github.com/chriskempson/base16-shell.git ~/.zsh/base16-shell" \
	"Cloning base16-shell repository"

execute "git clone git://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions" \
	"Cloning zsh-autosuggestions repository"
