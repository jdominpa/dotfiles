#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stow_packages() {

    declare -a DIRECTORIES_TO_CREATE=(

        ".config/nvim"
        ".emacs.d"
        ".zsh"

    )

    declare -a STOW_PKGS=(

        "emacs"
        "git"
        "neovim"
        "shell"
        "tmux"

    )

    local dir=""
    local pkg=""
    local source="$(cd .. && pwd)/stow"

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    print_in_purple "   Create directories\n\n"

    for dir in "${DIRECTORIES_TO_CREATE[@]}"; do

        dir="$HOME/$dir"
        execute "mkdir -p $dir" "$dir"

    done

    print_in_purple "\n   Stowing packages\n\n"

    for pkg in "${STOW_PKGS[@]}"; do

        execute "stow --dotfiles -d $source -t $HOME $pkg" "$pkg"

    done

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

main() {
    print_in_purple "\n • Create symbolic links\n\n"
    stow_packages "$@"
}

main "$@"
