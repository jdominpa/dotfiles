#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   GUI Programs\n\n"

install_package "Firefox" "firefox"

# Video player
install_package "VLC" "vlc"
install_package "ffmpeg" "ffmpeg"

# Image viewer
install_package "sxiv" "sxiv"

# Graphical file manager
install_package "pcmanfm" "pcmanfm"

# Pdf reader
install_package "Zathura" "zathura"
install_package "Zathura comic support" "zathura-cb"
install_package "Zathura DjVu support" "zathura-djvu"
install_package "Zathura Mupdf" "zathura-pdf-mupdf"
install_package "Poppler" "poppler"
