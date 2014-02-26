#!/usr/bin/env bash

PACKAGES=(
    "xserver-xorg"
    "build-essential"
    "hal"

    # CLI tools
    "xterm"
    "htop"
    "scrot"
    "unrar"
    "dlocate"
    "dnsutils"
    "alsa-utils"
    "feh"

    # Text editing
    "emacs"
    "emacs-goodies-el"
    "vim"

    # Appearance / fonts
    "ttf-dejavu"
    "ttf-mscorefonts-installer"
    "elementary-icon-theme"
    "gtk2-engines-murrine"
    "lxappearance"

    # For "import" screenshot command
    "imagemagick"

    # For slock
    "suckless-tools"

    # Thunar + plugins
    "thunar"
    "thunar-volman"
    "thunar-archive-plugin"

    # Window management + apps
    "awesome"
    "awesome-extra"
    "clipit"
    "keepassx"
    "leafpad"
    "mirage"
    "xdiskusage"

    #
    # Other non-essential goodies
    #

    # For recovering deleted files from disk
    # "foremost"
    # "scalpel"

    # For making bootable usb sticks
    # "unetbootin"

    # For 32-bit acroread
    # "libxml2:i386"
    # "acroread"

    # For VLC + media codecs
    # "ubuntu-restricted-extras"
    # "vlc"
)

sudo apt-get install `(IFS=" "; echo "${PACKAGES[*]}")`
