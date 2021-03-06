#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

orphans() {

    # Remove packages that were automatically installed to satisfy
    # dependencies for other packages and are no longer needed.

    execute \
        "sudo pacman --noconfirm -Rns $(pacman -Qtdq) &> /dev/null" \
        "Removing orphans"

}

install_package() {

    declare -r EXTRA_ARGUMENTS="$3"
    declare -r PACKAGE="$2"
    declare -r PACKAGE_READABLE_NAME="$1"

    if ! package_is_installed "$PACKAGE"; then
        execute "sudo pacman --needed --noconfirm -S $EXTRA_ARGUMENTS $PACKAGE > /dev/null" "$PACKAGE_READABLE_NAME"
        #                                  │                  suppress output ─┘
        #           assume "yes" as the answer to all prompts
    else
        print_success "$PACKAGE_READABLE_NAME"
    fi

}

package_is_installed() {
    pacman -Qqe "$1" &> /dev/null
}

update_and_upgrade() {

    # Resynchronize the package index files from their sources.

    execute \
        "sudo pacman -Syu" \
        "Pacman update"

}

install_yay() {

    if ! cmd_exists "yay -Pd"; then
        initialDir="$(pwd)"
        mkdir -p ~/Programs
        printf "\n" | git clone https://aur.archlinux.org/yay.git ~/Programs/yay && cd ~/Programs/yay && makepkg -sicr; exit=$?; cd "$initialDir"
        #  └─ simulate the ENTER keypress
    fi

    print_result "$exit" "Yay"

}

install_aur_package() {

    declare -r PACKAGE="$2"
    declare -r PACKAGE_READABLE_NAME="$1"

    if ! aur_package_is_installed "$PACKAGE"; then
        yay --needed -S "$PACKAGE"
    else
        print_success "$PACKAGE_READABLE_NAME"
    fi

}

aur_package_is_installed() {
    yay -Qqe "$1" &> /dev/null
}
