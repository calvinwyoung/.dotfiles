#!/usr/bin/env bash

PACKAGES=(
    "coreutils"
    "findutils"
    "grep"
    "bash-completion"
    "p5-file-rename"
    "git-core"
    "watch"
    "emacs"
    "emacs-app"
    "unrar"
    "python27"
)

VARIANTS=(
    "+bash_completion"
)

JOINED_PACKAGES=`(IFS=" "; echo "${PACKAGES[*]}")`
JOINED_VARIANTS=`(IFS=" "; echo "${VARIANTS[*]}")`
sudo port install $JOINED_PACKAGES $JOINED_VARIANTS

# Switch to macports python.
sudo port select --set python python27

# To use bash completion, we need to switch to the macports version of bash.
# This requires that we first add the PATH for the macports bash to /etc/shells.
if ! grep -Fxq "/opt/local/bin/bash" /etc/shells; then
    echo "/opt/local/bin/bash" | sudo tee -a /etc/shells
fi
chsh -s /opt/local/bin/bash
