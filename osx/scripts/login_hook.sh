#!/bin/bash
# This file will be executed on login and should be enabled with:
#    sudo defaults write com.apple.loginwindow LoginHook /path/to/login_hook.sh


# Disable mouse acceleration for external mice
defaults write .GlobalPreferences com.apple.mouse.scaling -1
