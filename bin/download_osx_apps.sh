#!/usr/bin/env bash

URLS=(
    # OS X Command Line Tools
    "https://developer.apple.com/downloads"

    # MacPorts
    "http://www.macports.org/install.php"

    # Hotkeys and window management
    "https://pqrs.org/macosx/keyremap4macbook/"
    "https://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en"
    "https://github.com/jigish/slate"

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