#!/bin/bash
#
# This file will be executed on login and should be run using launchd.

# Disable mouse acceleration for external mice
defaults write .GlobalPreferences com.apple.mouse.scaling -1
