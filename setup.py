#!/usr/bin/env python
"""Setup script to install the dotfiles for the current user."""

import os
import shutil
import subprocess

BLACKLIST = ["setup.py", "README.md"]

HOME_DIR = os.path.expanduser("~")
DOTFILES_DIR = os.path.dirname(os.path.abspath(__file__))

def main():
    # Make sure all submodules are initialized and updated
    subprocess.call(["git", "submodule", "init"])
    subprocess.call(["git", "submodule", "update"])

    for filename in os.listdir(DOTFILES_DIR):
        if filename.startswith("."):
            # Skip hidden files and directories
            continue
        elif filename in BLACKLIST:
            # Skip blacklisted files and directories
            continue

        symlink_path = os.path.join(HOME_DIR, ".%s" % filename)
        source_path = os.path.join(DOTFILES_DIR, filename)

        # If the symlink path already exists, then notify the user before
        # overwriting anything.
        if os.path.exists(symlink_path):
            # If this dotfile was already previously installed, then we're free
            # to skip it.
            if os.path.islink(symlink_path):
                if os.readlink(symlink_path) == source_path:
                    continue

            response = raw_input("Overwrite file %s? [Y/n] " % symlink_path)
            if not response or response.lower() == "y":
                if os.path.isfile(symlink_path):
                    os.remove(symlink_path)
                else:
                    shutil.rmtree(symlink_path)
            else:
                print "Skipping %s..." % symlink_path
                continue

        # Finally, create a symlink pointing to this dotfile
        os.symlink(source_path, symlink_path)
        print "Installed file %s -> %s" % (symlink_path, source_path)

if __name__ == "__main__":
    main()
