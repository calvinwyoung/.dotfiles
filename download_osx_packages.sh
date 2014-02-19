#!/usr/bin/env bash

# sudo port install coreutils
# sudo port install findutils
# sudo port install grep
# sudo port install bash-completion
# sudo port install p5-file-rename
# sudo port install git-core +bash_completion
# sudo port install watch
# sudo port install emacs
# sudo port install emacs-app
# sudo port install unrar
# sudo port install python27
# sudo port install py27-ipython
# sudo port select --set python python27
# sudo port select --set ipython ipython27

URLS=(
    # MacPorts
    "http://www.macports.org/install.php"

    # Hotkeys and window management
    "https://pqrs.org/macosx/keyremap4macbook/"
    "https://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en"
    "https://github.com/jigish/slate"

    # Emacs
    "http://emacsformacosx.com/emacs-builds/Emacs-24.3-universal-10.6.8.dmg"

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
)

for url in "${URLS[@]}"
do
    open $url
done
