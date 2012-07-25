#!/usr/bin/env python
"""Setup script to install the dotfiles for the current user."""

import os
import shutil
import subprocess

BLACKLIST = ["setup.py", "README.md"]

HOME_DIR = os.path.expanduser("~")
DOTFILES_DIR = os.path.dirname(os.path.abspath(__file__))

def create_symlink(source, link_name):
    """Creates a symbolic link named `link_name` pointing to `source`.

    This method also prompts the user before overwriting any existing files.

    Args:
        source, str: the path to the symlink source
        link_name, str: the name of the symlink to be created
    """
    # If the symlink path already exists, then notify the user before
    # overwriting anything.
    if os.path.exists(link_name):
        # If an identical symlink already exists, then we're free to skip it.
        if os.path.islink(link_name) and os.readlink(link_name) == source:
            return

        response = raw_input("Overwrite file %s? [Y/n] " % link_name)
        if not response or response.lower() == "y":
            if os.path.isfile(link_name):
                os.remove(link_name)
            else:
                shutil.rmtree(link_name)
        else:
            print "Skipping %s..." % link_name
            return

    # Finally, create a symlink pointing to this dotfile
    os.symlink(source, link_name)
    print "Installed file %s -> %s" % (link_name, source)

def main():
    # Make sure all submodules are initialized and updated
    subprocess.call(["git", "submodule", "init"])
    subprocess.call(["git", "submodule", "update"])

    # Install dotfiles in top-level directory, skipping the "config" directory.
    for filename in os.listdir(DOTFILES_DIR):
        if filename.startswith("."):
            # Skip hidden files and directories
            continue
        elif filename in BLACKLIST + ["config"]:
            # Skip blacklisted files and directories. Also skip the "config"
            # directory, which we handle separately below.
            continue

        create_symlink(os.path.join(DOTFILES_DIR, filename),
                       os.path.join(HOME_DIR, ".%s" % filename))

    # The "config" directory is special. All files/directories in here should
    # have a symlink created inside the ~/.config directory. First, we check if
    # the ~/.config directory exists, and create it if necessary
    config_dir = os.path.join(HOME_DIR, ".config")
    if not os.path.exists(config_dir):
        os.mkdir(config_dir, 0700)

    # Now we install the symlinks for the "config" directory.
    for filename in os.listdir(os.path.join(DOTFILES_DIR, "config")):
        if filename.startswith("."):
            # Skip hidden files and directories
            continue

        create_symlink(
            os.path.join(os.path.join(DOTFILES_DIR, "config"), filename),
            os.path.join(config_dir, "%s" % filename))

if __name__ == "__main__":
    main()
