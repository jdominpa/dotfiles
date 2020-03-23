#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

create_symlinks() {

    declare -a HOME_FILES_TO_SYMLINK=(

        "linux_config/icons"
        "linux_config/gtkrc-2.0"
        "linux_config/inputrc"
        "linux_config/profile"
        "linux_config/xinitrc"
        "linux_config/xprofile"

    )

    declare -a NOT_HOME_FILES_TO_SYMLINK=(

        "linux_config/config/bspwm"
        "linux_config/config/calcurse"
        "linux_config/config/dunst"
        "linux_config/config/fontconfig"
        "linux_config/config/gtk-3.0"
        "linux_config/config/libfm"
        "linux_config/config/pcmanfm"
        "linux_config/config/polybar"
        "linux_config/config/suckless"
        "linux_config/config/sxhkd"
        "linux_config/config/transmission-daemon"
        "linux_config/config/zathura"
        "linux_config/config/emoji"
        "linux_config/config/fontawesome"
        "linux_config/config/mimeapps.list"
        "linux_config/local/bin"

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
