# Dotfiles
To install, execute `make darwin` or `make linux` based the system on which
these dotfiles are being installed.  Each of these commands will implicitly call
`make common` to install dotfiles that are relevant to both systems.

This script creates a symlink from ``~/.[name]`` for each file in the 
corresponding system directory.  If a file named ``~/.[name]`` already exists, 
then we prompt the user before overwriting it. If ``[name]`` contains a 
double-underscore (i.e., `__`), then we assume it indicates a directory partition.
Therefore, the file ``parent__child`` will be symlinked from ``~/.parent/child``.
