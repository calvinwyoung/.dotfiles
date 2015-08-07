#!/usr/bin/env bash

PREFERENCES_DIR=$HOME/Documents/.Settings/OSX__Library__Preferences

for filepath in $PREFERENCES_DIR/*; do
    echo "Installing symlink for $HOME/Library/Preferences/$(basename $filepath)"
    ln -sf $filepath $HOME/Library/Preferences/$(basename $filepath)
done

# Also sync Alfred preferences, which are managed separately.
echo "Installing Alfred preferences"
ALFRED_SOURCE="$HOME/Documents/.Settings/OSX__Library__Application_Support__Alfred_2/Alfred.alfredpreferences"
ALFRED_TARGET="$HOME/Library/Application Support/Alfred 2/Alfred.alfredpreferences"
if [ -d "$ALFRED_TARGET" ]; then
    rm -rf "$ALFRED_TARGET"
fi
ln -sf "$ALFRED_SOURCE" "$ALFRED_TARGET"
