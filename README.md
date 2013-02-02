# Dotfiles
To install, simply execute the ``install.py`` script.

This script adds a symlink from ``~/.[name]`` to each ``[name]`` in the
``.dotfiles`` directory.  If ``~/.[name]`` already exists then we prompt the
user before overwriting it. If ``[name]`` contains a double-underscore (i.e.,
"__"), then we assume the part of the filename following the double-underscore
is a subdirectory. Therefore, the file ``parent__child`` will be installed at
``~/.parent/child``.
