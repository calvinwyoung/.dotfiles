#!/usr/bin/env bash

URLS=(
    # Hotkeys and window management
    "https://pqrs.org/osx/karabiner/"
    "https://github.com/Hammerspoon/hammerspoon"

    # iTerm2
    "http://www.iterm2.com/#/section/home"

    # MacPass
    "https://macpassapp.org/"

    # Alfred
    "https://www.alfredapp.com/"

    # Google Drive
    "https://tools.google.com/dlpage/drive/?hl=en"

    # Dropbox
    "https://www.dropbox.com/downloading"

    # Easy Move Resize
    "https://github.com/dmarcotte/easy-move-resize"
)

for url in "${URLS[@]}"
do
    open $url
done
