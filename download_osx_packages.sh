#!/usr/bin/env bash

URLS=(
    # Hotkeys and window management
    "https://pqrs.org/macosx/keyremap4macbook/files/KeyRemap4MacBook-9.2.0.dmg"
    "https://pqrs.org/macosx/keyremap4macbook/files/PCKeyboardHack-10.5.0.dmg"
    "http://slate.ninjamonkeysoftware.com/Slate.dmg"

    # Dev tools
    "https://git-osx-installer.googlecode.com/files/git-1.8.4.2-intel-universal-snow-leopard.dmg"
    "http://emacsformacosx.com/emacs-builds/Emacs-24.3-universal-10.6.8.dmg"
    "http://iterm2.com/downloads/stable/iTerm2_v1_0_0.zip"

    # SSHFS
    "http://sourceforge.net/projects/osxfuse/files/osxfuse-2.6.2/osxfuse-2.6.2.dmg/download"
    "https://github.com/osxfuse/sshfs/releases/download/osxfuse-sshfs-2.5.0/sshfs-2.5.0.pkg"

    # KeePassX
    "http://www.keepassx.org/releases/KeePassX-0.4.3.dmg"

    # Chrome
    "https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg"
)

for url in "${URLS[@]}"
do
    echo "Downloading $url"
    (cd ~/Downloads && curl -O $url)
done
