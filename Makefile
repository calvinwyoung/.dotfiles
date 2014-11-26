OSX_PREFS_SYNC_DIR = ~/Documents/App\ Storage/OSX__Library__Preferences

.PHONY: common darwin linux clean

common:
	bin/create_symlinks.py common/
	git submodule update --init --recursive

linux: common
	bin/create_symlinks.py linux/

darwin: common
	bin/create_symlinks.py darwin/

# Create a symlink for our Preferences directory. Since the `ln` command can't
# overwrite directories, we must remove the original directory (but only if it's
# a real directory and not a symlink). Use the `-f` and `-n` flags to make `ln`
# overwrite old symlinks without dereferencing them.
	if [ ! -h ~/Library/Preferences ]; then rm -rf ~/Library/Preferences; fi
	ln -sfn $(OSX_PREFS_SYNC_DIR) ~/Library/Preferences

# Apply custom default settings.
	bin/set_osx_defaults.sh

# Enable our login_init hook.
	launchctl load ~/Library/LaunchAgents/login_init.plist

clean:
	bin/create_symlinks.py -l common/  | tr "\n" "\0" | xargs -0 rm -f
	bin/create_symlinks.py -l linux/ | tr "\n" "\0" | xargs -0 rm -f
	bin/create_symlinks.py -l darwin/ | tr "\n" "\0" | xargs -0 rm -f
