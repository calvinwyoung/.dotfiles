#!/usr/bin/env python
"""Setup script to install the dotfiles for the current user.

For each file/subdirectory in the current directory, this script installs a
symlink in the user's home directory named ".[filename]". If the filename
contains a double-underscore (i.e., "__"), the part of the filename following
the double-underscore will be interpreted as subdirectory. Therefore, for a file
named "parent__child", this script will install a symlink at "~/.parent/child"
that points to "parent__child".
"""

import argparse
import os
import shutil
import subprocess
import sys


HOME_DIR = os.path.expanduser("~")


def create_symlink(link_name, source, simulate):
    """Creates a symbolic link named `link_name` pointing to `source`.

    This method also prompts the user before overwriting any existing files.

    Args:
        source, str: the path to the symlink source
        link_name, str: the name of the symlink to be created
    """
    print "Installing file %s -> %s" % (link_name, source)
    if simulate:
        return

    # If the symlink path already exists, then notify the user before
    # overwriting anything.
    if os.path.lexists(link_name):
        # If an identical symlink already exists, then we're free to skip it.
        if os.path.islink(link_name) and os.readlink(link_name) == source:
            return

        response = raw_input("Overwrite file %s? [Y/n] " % link_name)
        if not response or response.lower().strip() == "y":
            if os.path.isfile(link_name) or os.path.islink(link_name):
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
    print "  Done."

def main(directories, simulate=False):
    """Creates dotfile symlinks for files in the given directories.

    Args:
        directories, list[str]: list of directories containing dotfiles
        simulate, bool: if True, the only performs a simulation of creating a
            symlink without modifying the filesystem
    """
    for directory in directories:
        dotfile_dir = os.path.abspath(directory)
        for filename in os.listdir(dotfile_dir):
            # Skip hidden files.
            if filename.startswith("."):
                continue

            filename_parts = filename.split("__")
            filename_parts[0] = ".%s" % filename_parts[0]
            symlink_name = os.path.join(*filename_parts)

            create_symlink(
                os.path.join(HOME_DIR, symlink_name),
                os.path.join(dotfile_dir, filename),
                simulate)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create dotfile symlinks.")

    parser.add_argument("directories", nargs="+",
                        help="directories containing dotfiles")
    parser.add_argument("-s", "--simulate", action="store_true",
                        help=("performs a simulation of installing symlinks "
                              "without modifying the file system"))

    args = parser.parse_args()
    sys.exit(main(args.directories, args.simulate))
