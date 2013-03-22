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


def create_symlink(target, source):
    """Creates a symbolic link named `target` pointing to `source`.

    This method also prompts the user before overwriting any existing files.

    Args:
        source, str: the path to the symlink source
        target, str: the name of the symlink to be created
    """
    print "Installing file %s -> %s" % (target, source)

    # If the symlink path already exists, then notify the user before
    # overwriting anything.
    if os.path.lexists(target):
        # If an identical symlink already exists, then we're free to skip it.
        if os.path.islink(target) and os.readlink(target) == source:
            return

        response = raw_input("Overwrite file %s? [Y/n] " % target)
        if not response or response.lower().strip() == "y":
            if os.path.isfile(target) or os.path.islink(target):
                os.remove(target)
            else:
                shutil.rmtree(target)
        else:
            print "Skipping %s..." % target
            return

    # Ensure that the directory to which the symlink will be written exists.
    elif not os.path.exists(os.path.dirname(target)):
        os.makedirs(os.path.dirname(target))

    # Finally, create the actual symlink.
    os.symlink(source, target)
    print "  Done."


def main(directories, list_only=False):
    """Creates dotfile symlinks for files in the given directories.

    Args:
        directories, list[str]: list of directories containing dotfiles
        list_only, bool: if True, then only prints a list of symlinks that would
            be created without modifying the filesystem
    """
    for directory in directories:
        dotfile_dir = os.path.abspath(directory)
        for filename in os.listdir(dotfile_dir):
            # Skip hidden files.
            if filename.startswith("."):
                continue

            filename_parts = filename.split("__")
            filename_parts[0] = ".%s" % filename_parts[0]
            target_path = os.path.join(HOME_DIR, *filename_parts)

            if list_only:
                print target_path
            else:
                create_symlink(target_path, os.path.join(dotfile_dir, filename))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create dotfile symlinks.")

    parser.add_argument("directories", nargs="+",
                        help="directories containing dotfiles")
    parser.add_argument("-l", "--list_only", action="store_true",
                        help=("lists the paths to all symlinks that would be "
                              "created"))

    args = parser.parse_args()

    sys.exit(main(args.directories, args.list_only))
