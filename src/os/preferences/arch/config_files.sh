#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

create_symlinks() {

    declare -a HOME_FILES_TO_SYMLINK=(

        "arch_config/icons"
        "arch_config/gtkrc-2.0"
        "arch_config/profile"
        "arch_config/xinitrc"
        "arch_config/xprofile"

    )

    declare -a NOT_HOME_FILES_TO_SYMLINK=(

        "arch_config/config/alacritty"
        #"arch_config/config/bspwm"
        "arch_config/config/dunst"
        "arch_config/config/fontconfig"
        "arch_config/config/gtk-3.0"
        "arch_config/config/libfm"
        "arch_config/config/pcmanfm"
        #"arch_config/config/polybar"
        "arch_config/config/suckless"
        #"arch_config/config/sxhkd"
        "arch_config/xmonad/xmobarrc.hs"
        "arch_config/xmonad/xmonad.hs"
        "arch_config/config/zathura"
        "arch_config/config/emoji"
        "arch_config/config/fontawesome"
        "arch_config/config/mimeapps.list"
        "arch_config/local/bin"

    )

    local i=""
    local sourceFile=""
    local targetFile=""
    local skipQuestions=false

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    skip_questions "$@" \
        && skipQuestions=true

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    for i in "${HOME_FILES_TO_SYMLINK[@]}"; do

        sourceFile="$(cd ../../../ && pwd)/$i"
        targetFile="$HOME/.$(printf "%s" "$i" | sed "s/.*\/\(.*\)/\1/g")"

        if [ ! -e "$targetFile" ] || $skipQuestions; then

            execute \
                "ln -fs $sourceFile $targetFile" \
                "$targetFile → $sourceFile"

        elif [ "$(readlink "$targetFile")" == "$sourceFile" ]; then
            print_success "$targetFile → $sourceFile"
        else

            if ! $skipQuestions; then

                ask_for_confirmation "'$targetFile' already exists, do you want to overwrite it?"
                if answer_is_yes; then

                    rm -rf "$targetFile"

                    execute \
                        "ln -fs $sourceFile $targetFile" \
                        "$targetFile → $sourceFile"

                else
                    print_error "$targetFile → $sourceFile"
                fi

            fi

        fi

    done

    for i in "${NOT_HOME_FILES_TO_SYMLINK[@]}"; do

        sourceFile="$(cd ../../../ && pwd)/$i"
        targetFile="$HOME/.$(printf "%s" "$i" | sed "s|[^/]*/||")"
        pathToSymlink="$(printf "%s" "$targetFile" | rev | sed "s|[^/]*/||" | rev)"
        if [ ! -d "$pathToSymlink" ]; then
                mkdir -p "$pathToSymlink"
        fi

        if [ ! -e "$targetFile" ] || $skipQuestions; then

            execute \
                "ln -fs $sourceFile $targetFile" \
                "$targetFile → $sourceFile"

        elif [ "$(readlink "$targetFile")" == "$sourceFile" ]; then
            print_success "$targetFile → $sourceFile"
        else

            if ! $skipQuestions; then

                ask_for_confirmation "'$targetFile' already exists, do you want to overwrite it?"
                if answer_is_yes; then

                    rm -rf "$targetFile"

                    execute \
                        "ln -fs $sourceFile $targetFile" \
                        "$targetFile → $sourceFile"

                else
                    print_error "$targetFile → $sourceFile"
                fi

            fi

        fi

    done

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

main() {
    print_in_purple "\n   Linux configuration files\n\n"
    create_symlinks "$@"
    execute "ln -s ~/.profile ~/.zprofile" \
        "Linking .zprofile to .profile"
}

main "$@"
