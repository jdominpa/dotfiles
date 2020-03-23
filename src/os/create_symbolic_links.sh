#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

create_symlinks() {

    declare -a HOME_FILES_TO_SYMLINK=(

       #"bash_shell/aliases/bash_aliases"
       #"bash_shell/autocomplete/$(get_os)/bash_autocomplete"
       #"bash_shell/colors/$(get_os)/bash_colors"
       #"bash_shell/bash_exports"
       #"bash_shell/bash_logout"
       #"bash_shell/bash_options"
       #"bash_shell/bash_profile"
       #"bash_shell/bash_prompt"
       #"bash_shell/bashrc"
       #"bash_shell/inputrc"
        "zsh_shell/zshrc"
        "shell/curlrc"

        "git/gitattributes"
        "git/gitconfig"
        "git/gitignore"

        "doom_emacs/doom.d"

    )

    declare -a NOT_HOME_FILES_TO_SYMLINK=(

    	"neovim/config/nvim/autoload"
    	"neovim/config/nvim/after"
    	"neovim/config/nvim/ftplugin"
    	"neovim/config/nvim/plugin"
    	"neovim/config/nvim/ultisnips"
        "neovim/config/nvim/init.vim"
        "neovim/config/nvim/coc-settings.json"

		"zsh_shell/zsh/aliases"
		"zsh_shell/zsh/colors"
		"zsh_shell/zsh/exports"
		"zsh_shell/zsh/completions"

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

        sourceFile="$(cd .. && pwd)/$i"
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

        sourceFile="$(cd .. && pwd)/$i"
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
    print_in_purple "\n • Create symbolic links\n\n"
    create_symlinks "$@"
}

main "$@"
