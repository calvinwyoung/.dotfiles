#!/usr/bin/env bash

URLS=(
    # OS X Command Line Tools
    "https://developer.apple.com/downloads"

    # Hotkeys and window management
    "https://pqrs.org/osx/karabiner/"
    "https://pqrs.org/osx/karabiner/seil.html.en"
    "https://github.com/Hammerspoon/hammerspoon"
    "http://coderage-software.com/zooom/download/download.html"

    # iTerm2
    "http://www.iterm2.com/#/section/home"

    # MacPass
    "https://macpassapp.org/"

    # Chrome
    "https://www.google.com/intl/en/chrome/browser/"

    # Google Drive
    "https://tools.google.com/dlpage/drive/?hl=en"

    # Dropbox
    "https://www.dropbox.com/downloading"
)

for url in "${URLS[@]}"
do
    open $url
done
