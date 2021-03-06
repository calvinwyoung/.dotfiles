.PHONY: common darwin linux clean

common:
	bin/create_symlinks.py common/

linux: common
	bin/create_symlinks.py linux/

darwin: common
	bin/create_symlinks.py darwin/

# Apply custom default settings.
	bin/set_osx_defaults.sh

# Enable our login_init hook.
	launchctl load ~/Library/LaunchAgents/login_init.plist

# Update karabiner configs
	cd darwin/config__karabiner/ && make

clean:
	bin/create_symlinks.py -l common/  | tr "\n" "\0" | xargs -0 rm -f
	bin/create_symlinks.py -l linux/ | tr "\n" "\0" | xargs -0 rm -f
	bin/create_symlinks.py -l darwin/ | tr "\n" "\0" | xargs -0 rm -f
