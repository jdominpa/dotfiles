#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

change_default_zsh() {

    # Set `Zsh` as default shell

    chsh -s "/bin/zsh" &> /dev/null
    print_result $? "Zsh (shell change)"

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

main() {

    print_in_purple "\n   Terminal components\n\n"

    install_package "Zsh" "zsh" \
        && change_default_zsh

    execute "mkdir -p $HOME/.zsh" \
        "Creating .zsh directory"

    execute "git clone https://github.com/chriskempson/base16-shell.git ~/.zsh/base16-shell" \
        "Cloning base16-shell repository"

    execute "git clone git://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions" \
        "Cloning zsh-autosuggestions repository"

    # Terminal emulator
    install_package "Alacritty" "alacritty"

}

main
