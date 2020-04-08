#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Miscellaneous\n\n"

# Firefox browser
install_package "Firefox" "firefox"

# Video player
install_package "mpv" "mpv"

# Image viewer
install_package "sxiv" "sxiv"

# Graphical file manager
install_package "pcmanfm" "pcmanfm"

# Pdf reader
install_package "Zathura" "zathura"
install_package "Zathura Mupdf" "zathura-pdf-mupdf"

# LaTeX packages
install_package "LaTeX" "texlive-most"
install_package "Languages" "texlive-lang"
install_package "Biber" "biber"
install_package "Pandoc" "pandoc"