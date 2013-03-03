#!/bin/bash

# Set login hook.
sudo defaults write com.apple.loginwindow LoginHook $HOME/.scripts/login_hook.sh

# Make Mission Control animations faster.
defaults write com.apple.dock expose-animation-duration -float 0.05; killall Dock

###############################################################################
# Finder                                                                      #
###############################################################################

# Don't show hidden files by default.
defaults write com.apple.Finder AppleShowAllFiles NO

# Show the ~/Library folder.
chflags nohidden ~/Library

# Set keyboard shortcuts:
#   - Disable "Hide Finder" keyboard shortcut so we can bind Command + H to toggle
#     showing hidden files.
#   - Set Command + L to open the "Go to Folder" dialog.
defaults write com.apple.finder NSUserKeyEquivalents \
    '{"Hide Finder"="\0"; "Show System Files"="@h"; "Go to Folder..."="@l";}'

###############################################################################
# Trackpad, mouse, keyboard, Bluetooth accessories, and input                 #
###############################################################################

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Trackpad: map bottom right corner to right-click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true

# Disable “natural” (Lion-style) scrolling
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false

# Always show scrollbars
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"

# Disable smooth scrolling
defaults write -g NSScrollAnimationEnabled -bool false

# Disable elastic scrolling
# http://osxdaily.com/2012/05/10/disable-elastic-rubber-band-scrolling-in-mac-os-x/
defaults write -g NSScrollViewRubberbanding -int 0

###############################################################################
# Screen                                                                      #
###############################################################################

# Require password immediately after sleep or screen saver begins
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0
