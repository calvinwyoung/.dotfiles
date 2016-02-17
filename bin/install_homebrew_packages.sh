#!/usr/bin/env bash

# Install homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Must install homebrew/dups in order to install some packages that override
# default system commands.
brew tap homebrew/dupes

# Install system software.
brew install bash-completion
brew install coreutils
brew install findutils --with-default-names
brew install grep --with-default-names
brew install rename

# Install additional applications.
brew install git
brew install unrar

# Need to install ispell for flyspell to work in emacs.
brew install ispell --with-lang-en
