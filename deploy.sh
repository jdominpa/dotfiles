#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"
set -e

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_color() {
    printf "%b" \
        "$(tput setaf "$2" 2> /dev/null)" \
        "$1" \
        "$(tput sgr0 2> /dev/null)"
}

print_in_green() {
    print_in_color "$1" 2
}

print_in_red() {
    print_in_color "$1" 1
}

print_in_blue() {
    print_in_color "$1" 6
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

deploy() {

    declare -a DIRECTORIES_TO_CREATE=(
        ".config/nvim"
        ".config/zsh"
        ".emacs.d"
    )

    declare -a STOW_PKGS=(
        "emacs"
        "xdg_config"
        "zshenv"
        "tmux"
    )

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    print_in_blue "[INFO] Creating directories...\n"

    for dir in "${DIRECTORIES_TO_CREATE[@]}"; do
        mkdir -p $(dirname "$HOME/$dir")
        print_in_green "[OK] $dir created\n"
    done

    print_in_blue "[INFO] Stowing packages...\n"
    local source="$(pwd)"

    for pkg in "${STOW_PKGS[@]}"; do
        if $(stow --dotfiles --dir=$source --target=$HOME $pkg); then
            print_in_green "[OK] Stowed package: $pkg\n"
        else
            print_in_red "[ERROR] Could not stow package: $pkg\n"
        fi
    done

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

deploy "$@"
