#!/usr/bin/env python
"""Setup script to install the dotfiles for the current user.

For each file/subdirectory in the current directory, this script installs a
symlink in the user's home directory named ".[filename]". If the filename
contains a double-underscore (i.e., "__"), the part of the filename following
the double-underscore will be interpreted as subdirectory. Therefore, for a file
named "parent__child", this script will install a symlink at "~/.parent/child"
that points to "parent__child".
"""

import os
import shutil
import subprocess

BLACKLIST = ["install.py", "README.md"]

HOME_DIR = os.path.expanduser("~")
DOTFILES_DIR = os.path.dirname(os.path.abspath(__file__))


def create_symlink(link_name, source):
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
        if not response or response.lower().strip() == "y":
            if os.path.isfile(link_name):
                os.remove(link_name)
            else:
                shutil.rmtree(link_name)
        else:
            print "Skipping %s..." % link_name
            return

    # Ensure that the directory to which the symlink will be written exists.
    elif not os.path.exists(os.path.dirname(link_name)):
        os.makedirs(os.path.dirname(link_name))

    # Finally, create the actual symlink.
    os.symlink(source, link_name)
    print "Installed file %s -> %s" % (link_name, source)


def main():
    # Make sure all submodules are initialized and updated
    subprocess.call(["git", "submodule", "update", "--init", "--recursive"])

    # Iterate over all dotfiles and create symlinks to them in the home
    # directory.
    for filename in os.listdir(DOTFILES_DIR):
        if filename.startswith("."):
            continue
        elif filename in BLACKLIST:
            continue

        filename_parts = filename.split("__")
        symlink_name = ".%s" % os.path.join(*filename_parts)

        create_symlink(
            os.path.join(HOME_DIR, symlink_name),
            os.path.join(DOTFILES_DIR, filename))


if __name__ == "__main__":
    main()
