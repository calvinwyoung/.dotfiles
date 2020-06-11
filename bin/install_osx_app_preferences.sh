#!/usr/bin/env bash

SETTINGS_ROOT_DIR=$HOME/Dropbox/_Settings

PREFERENCES_DIR=$SETTINGS_ROOT_DIR/OSX__Library__Preferences

for filepath in $PREFERENCES_DIR/*; do
    echo "Installing symlink for $HOME/Library/Preferences/$(basename $filepath)"
    ln -sf $filepath $HOME/Library/Preferences/$(basename $filepath)
done

