#!/usr/bin/env bash

PREFERENCES_DIR=$HOME/Documents/.Settings/OSX__Library__Preferences

for filepath in $PREFERENCES_DIR/*; do
    echo "Installing symlink for $HOME/Library/Preferences/$(basename $filepath)"
    ln -sf $filepath $HOME/Library/Preferences/$(basename $filepath)
done

# Also sync Alfred preferences, which are managed separately.
alfred_source="$HOME/Documents/.Settings/OSX__Library__Application_Support__Alfred_2/Alfred.alfredpreferences"
alfred_target="$HOME/Library/Application Support/Alfred 2/Alfred.alfredpreferences"
if [ -d "$alfred_target" ]; then
    rm -rf "$alfred_target"
fi
ln -sf "$alfred_source" "$alfred_target"
