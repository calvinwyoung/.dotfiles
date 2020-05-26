#!/usr/bin/env python3
"""Setup script to install the dotfiles in the given directories.

This script creates a symlink from `~/.[name]` for each file in the
corresponding system directory.  If a file named `~/.[name]` already exists,
then we prompt the user before overwriting it. This script interpretes all
double-udnerscores (i.e., `__`) in teh file name as directory partitions.
partition. Therefore, a file named `parent__child` will be installed to
`~/.parent/child`.
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
    print(f'Installing file {target} -> {source}')

    # If the symlink path already exists, then notify the user before
    # overwriting anything.
    if os.path.lexists(target):
        # If an identical symlink already exists, then we're free to skip it.
        if os.path.islink(target) and os.readlink(target) == source:
            return

        response = input("Overwrite file %s? [Y/n] " % target)
        if not response or response.lower().strip() == "y":
            if os.path.isfile(target) or os.path.islink(target):
                os.remove(target)
            else:
                shutil.rmtree(target)
        else:
            print(f'Skipping {target}...')
            return

    # Ensure that the directory to which the symlink will be written exists.
    elif not os.path.exists(os.path.dirname(target)):
        os.makedirs(os.path.dirname(target))

    # Finally, create the actual symlink.
    os.symlink(source, target)
    print('  Done.')


def main(directories, list_symlinks=False):
    """Creates dotfile symlinks for files in the given directories.

    Args:
        directories, list[str]: list of directories containing dotfiles
        list_symlinks, bool: if True, then only prints a list of symlinks that
           would be created without modifying the filesystem
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

            if list_symlinks:
                print(target_path)
            else:
                create_symlink(target_path, os.path.join(dotfile_dir, filename))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create dotfile symlinks.")

    parser.add_argument(
        "directories",
        nargs="+",
        help="directories containing dotfiles")
    parser.add_argument(
        "-l",
        "--list_symlinks",
        action="store_true",
        help=("prints a list of symlinks that would be created without making "
              "any changes to the file system"))

    args = parser.parse_args()

    sys.exit(main(args.directories, args.list_symlinks))
