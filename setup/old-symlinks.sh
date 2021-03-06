#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

create_symlinks() {

    declare -a HOME_FILES_TO_SYMLINK=(

        "git/gitattributes"
        "git/gitconfig"
        "git/gitignore"
        "shell/curlrc"
        #"tmux/tmux.conf"
        "shell/zshenv"

    )

    declare -a NOT_HOME_FILES_TO_SYMLINK=(

        "emacs/emacs.d/core"
        "emacs/emacs.d/early-init.el"
        "emacs/emacs.d/init.el"
        "emacs/emacs.d/modules"
        "emacs/emacs.d/personal-packages"
        "emacs/emacs.d/snippets"

        "neovim/config/nvim/after"
        "neovim/config/nvim/autoload"
        "neovim/config/nvim/ftplugin"
        "neovim/config/nvim/init.vim"
        "neovim/config/nvim/lua/jdominpa"
        "neovim/config/nvim/plugin"
        "neovim/config/nvim/ultisnips"

        "shell/zsh/aliases"
        "shell/zsh/colors"
        "shell/zsh/completions"
        "shell/zsh/.zshrc"

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

        sourceFile="$(cd .. && pwd)/stow/$i"
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

        sourceFile="$(cd .. && pwd)/stow/$i"
        targetFile="$HOME/.$(printf "%s" "$i" | sed "s|[^/]*/||")"
        pathToSymlink="$(printf "%s" "$targetFile" | rev | sed "s|[^/]*/||" | rev)"

		mkdir -p "$pathToSymlink"

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
    print_in_purple "\n • Create symbolic links\n\n"
    create_symlinks "$@"
}

main "$@"
