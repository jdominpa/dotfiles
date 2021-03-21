#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stow_symlinks() {

    declare -a DIRECTORIES_TO_CREATE=(

        ".icons"
        ".local"
        ".xmonad"

    )

    local dir=""
    local pkg=""
    local source="$(cd ../../../ && pwd)/stow"

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    print_in_purple "   Create directories\n\n"

    for dir in "${DIRECTORIES_TO_CREATE[@]}"; do

        dir="$HOME/$dir"
        execute "mkdir -p $dir" "$dir"

    done

    print_in_purple "\n   Stowing packages\n\n"

    execute "stow --dotfiles -d $source -t ~ arch" \
            "Arch configuration files"

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

main() {
    print_in_purple "\n • Arch configuration files\n\n"
    stow_symlinks "$@"
}

main "$@"
