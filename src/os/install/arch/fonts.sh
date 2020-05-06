#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Fonts\n\n"

install_package "Roboto" "ttf-roboto"
install_package "Roboto Mono" "ttf-roboto-mono"
install_package "Liberation" "ttf-liberation"
install_package "Noto fonts" "noto-fonts"
install_package "Noto cjk fonts" "noto-fonts-cjk"
install_package "Noto emoji fonts" "noto-fonts-emoji"
install_package "Noto extra fonts" "noto-fonts-extra"
install_package "Awesome font" "ttf-font-awesome"
