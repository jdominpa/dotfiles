#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Fonts\n\n"

# Fonts
install_package "Source code pro" "adobe-source-code-pro-fonts"
install_package "Source sans pro" "adobe-source-sans-pro-fonts"
install_package "Source serif pro" "adobe-source-serif-pro-fonts"
install_package "Noto fonts" "noto-fonts"
install_package "Noto cjk fonts" "noto-fonts-cjk"
install_package "Noto emoji fonts" "noto-fonts-emoji"
install_package "Noto extra fonts" "noto-fonts-extra"
install_package "Dejavu fonts" "ttf-dejavu"
install_package "Awesome font" "ttf-font-awesome"
