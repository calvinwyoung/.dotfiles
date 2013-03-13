#!/bin/bash

# Disable "New Window" animations.
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool NO

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true

# Disable the “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

###############################################################################
# Dock, Dashboard, Mission Control                                            #
###############################################################################

# Make Mission Control animations faster.
defaults write com.apple.dock expose-animation-duration -float 0.05

# Don’t show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true

# Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# Remove the auto-hiding Dock delay
defaults write com.apple.dock autohide-delay -float 0

# Remove the animation when hiding/showing the Dock
defaults write com.apple.dock autohide-time-modifier -float 0

###############################################################################
# Finder                                                                      #
###############################################################################

# Don't show hidden files by default.
defaults write com.apple.Finder AppleShowAllFiles NO

# Show all filename extensions.
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Show path bar
defaults write com.apple.finder ShowPathBar -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Remove the spring loading delay for directories
defaults write NSGlobalDomain com.apple.springing.delay -float 0

# Hide standard folders.
chflags hidden ~/Library
chflags hidden ~/Documents
chflags hidden ~/Movies
chflags hidden ~/Pictures
chflags hidden ~/Music
chflags hidden ~/Public

# Set keyboard shortcuts:
#   - Command + Shift + . to toggle showing hidden files.
#   - Command + L to open the "Go to Folder" dialog.
defaults write com.apple.finder NSUserKeyEquivalents \
    '{"Show System Files"="@$."; "Go to Folder..."="@l";}'

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

###############################################################################
# Kill affected applications                                                  #
###############################################################################

for app in "Dock" "Finder"; do
    echo "Restargin $app"
    killall "$app" > /dev/null 2>&1
done
echo "Done restarting apps. Note that some changes require restarting to take effect."
