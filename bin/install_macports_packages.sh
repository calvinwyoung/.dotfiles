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
    "py27-ipython"
)

VARIANTS=(
    "+bash_completion"
)

JOINED_PACKAGES=`(IFS=" "; echo "${PACKAGES[*]}")`
JOINED_VARIANTS=`(IFS=" "; echo "${VARIANTS[*]}")`
sudo port install $JOINED_PACKAGES $JOINED_VARIANTS

sudo port select --set python python27
sudo port select --set ipython ipython27
