# Dotfiles
To install, simply execute the SETUP script.

This script adds a symlink from ~/.*[name]* to each *[name]* in the .dotfiles directory.

If .*[name]* already exists in the home directory and is a symlink, then we simply update the symlink. Otherwise, if .*[name]* is a file or a directory, then we diff the .*[name]* in the home directory with the *[name]* in this directory and ask the user whether to replace it.
