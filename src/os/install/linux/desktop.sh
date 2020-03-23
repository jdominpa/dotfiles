#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Desktop components\n\n"

# Graphical server and window manager
install_package "xorg" "xorg"
install_package "xorg-xinit" "xorg-xinit"
install_package "Compton" "compton"
install_package "bspwm" "bspwm"
install_package "sxhkd" "sxhkd"
install_package "dmenu" "dmenu"

# Screen locker, wallpaper and night mode
install_package "Slock" "slock"
install_package "Redshift" "redshift"
install_package "Nitrogen" "nitrogen"

# File system compatibility
install_package "Dos filesystems" "dosfstools"
install_package "Ntfs filesystem" "ntfs-3g"
install_package "Mounter (udisks2)" "udisks2"

# Sound programs
install_package "Pulseaudio" "pulseaudio"
install_package "Pulseaudio-alsa" "pulseaudio-alsa"
install_package "Pulsemixer" "pulsemixer"

# Notification system
install_package "Dunst" "dunst"
install_package "Library for dunst" "libnotify"

# Fonts
install_package "Source code pro" "adobe-source-code-pro-fonts"
install_package "Source sans pro" "adobe-source-sans-pro-fonts"
install_package "Source serif pro" "adobe-source-serif-pro-fonts"
install_package "Noto emoji fonts" "noto-fonts-emoji"
install_package "Awesome font" "ttf-font-awesome"

# Custom cursor
install_package "Capitaine cursors" "capitaine-cursors"

# Printer programs
install_package "Printing server" "cups"
install_package "HP printer's drivers" "hplip"
install_package "Scanning program" "sane"
execute "sudo systemctl enable --now org.cups.cupsd.socket" \
    "Activating CUPS"
