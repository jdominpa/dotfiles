#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

change_default_zsh() {

    # Set `Zsh` as default shell

    chsh -s "/bin/zsh"
    print_result $? "Zsh (shell change)"

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

main() {

    print_in_purple "\n   Zsh shell\n\n"

    install_package "Zsh" "zsh" \
        && change_default_zsh

    execute "mkdir -p $HOME/.config/zsh" \
        "Creating zsh config directory"

    execute "git clone https://github.com/zsh-users/zsh-autosuggestions.git $HOME/.config/zsh/zsh-autosuggestions" \
        "Cloning zsh-autosuggestions repository"

}

main
