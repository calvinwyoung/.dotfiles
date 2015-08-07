#!/usr/bin/env bash

PREFERENCES_DIR=$HOME/Documents/.Settings/OSX__Library__Preferences

for filepath in $PREFERENCES_DIR/*; do
    echo "Installing symlink for $HOME/Library/Preferences/$(basename $filepath)"
    ln -sf $filepath $HOME/Library/Preferences/$(basename $filepath)
done

# Also sync Alfred preferences, which are managed separately.
if [ -d $HOME/Library/Application\ Support/Alfred\ 2 ]; then
    rm -rf $HOME/Library/Application\ Support/Alfred\ 2
fi
ln -sf $HOME/Documents/.Settings/OSX__Library__Application_Support__Alfred_2/* $HOME/Library/Application\ Support/Alfred\ 2/
