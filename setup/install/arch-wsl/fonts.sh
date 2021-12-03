#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Fonts\n\n"

install_package "Fira Code" "ttf-fira-code"
install_package "Iosevka" "ttc-iosevka"
install_package "Roboto" "ttf-roboto"
install_package "Roboto Mono" "ttf-roboto-mono"
install_package "Source sans font" "adobe-source-sans-pro-fonts"
install_package "Liberation" "ttf-liberation"
install_package "Noto fonts" "noto-fonts"
install_package "Noto cjk fonts" "noto-fonts-cjk"
install_package "Noto emoji fonts" "noto-fonts-emoji"
install_package "Noto extra fonts" "noto-fonts-extra"
install_package "Awesome font" "ttf-font-awesome"
install_package "Icon terminal font" "awesome-terminal-fonts"
