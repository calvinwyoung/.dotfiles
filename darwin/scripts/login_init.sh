#!/bin/bash
#
# This file will be executed on login and should be run using launchd.

# Disable mouse acceleration for external mice
defaults write .GlobalPreferences com.apple.mouse.scaling -1

# Limit ClipMenu to 5% CPU usage
# ~/.scripts/cpulimit_clipmenu.sh &
