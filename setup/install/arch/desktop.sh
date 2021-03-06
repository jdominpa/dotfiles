#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Desktop components\n\n"

# Graphical server and window manager
install_package "xorg" "xorg"
install_package "xorg-xinit" "xorg-xinit"
install_package "arandr" "arandr"
install_package "Picom" "picom"
install_package "Xmonad" "xmonad"
install_package "Xmonad-contrib" "xmonad-contrib"
install_package "Xmobar" "xmobar"
install_package "dmenu" "dmenu"

# Screen locker, wallpapers, night mode and hide mouse
install_package "Slock" "slock"
install_package "Redshift" "redshift"
install_package "Nitrogen" "nitrogen"
install_package "Unclutter" "unclutter"

# File system compatibility
install_package "Dos filesystems" "dosfstools"
install_package "Ntfs filesystem" "ntfs-3g"
install_package "exFAT filesystems" "exfat-utils"
install_package "Mounter (udisks2)" "udisks2"
install_package "Automounter (udiskie)" "udiskie"

# Sound
install_package "Pulseaudio" "pulseaudio"
install_package "Pulseaudio-alsa" "pulseaudio-alsa"
install_package "Pavucontrol" "pavucontrol"
install_package "Systray volume icon" "volumeicon"
install_package "Alsa utilities" "alsa-utils"

# Bluetooth
install_package "Pulseaudio bluetooth" "pulseaudio-bluetooth"
install_package "Bluez" "bluez"
install_package "Bluez utils" "bluez-libs bluez-utils"
install_package "Blueberry" "blueberry"

# Network programs
install_package "Systray network icon" "network-manager-applet"

# Notification system
install_package "Dunst" "dunst"
install_package "Library for dunst" "libnotify"

# System tray
install_package "System tray" "trayer"

# Num lock activation program
install_package "Numlockx" "numlockx"

# Screenshot program
install_package "Scrot" "scrot"

# Custom cursor
install_package "Capitaine cursors" "capitaine-cursors"

# Printer programs
install_package "Printing server" "cups"
install_package "Drivers to print to PDF" "cups-pdf"
install_package "HP printer's drivers" "hplip"
install_package "Scanning program" "sane"
