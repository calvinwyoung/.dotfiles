#!/usr/bin/env bash

URLS=(
    # OS X Command Line Tools
    "https://developer.apple.com/downloads"

    # Hotkeys and window management
    "https://pqrs.org/osx/karabiner/"
    "https://pqrs.org/osx/karabiner/seil.html.en"
    "https://github.com/Hammerspoon/hammerspoon"
    "http://coderage-software.com/zooom/download/download.html"
    "https://bahoom.com/hyperswitch"

    # iTerm2
    "http://www.iterm2.com/#/section/home"

    # SSHFS
    "http://osxfuse.github.io/"

    # KeePassX
    "http://www.keepassx.org/downloads"

    # iStatMenus
    "http://bjango.com/mac/istatmenus/"

    # Chrome
    "https://www.google.com/intl/en/chrome/browser/"

    # Google Drive
    "https://tools.google.com/dlpage/drive/?hl=en"
)

for url in "${URLS[@]}"
do
    open $url
done
