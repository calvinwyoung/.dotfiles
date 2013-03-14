.PHONY: common linux darwin install_linux_packages

common:
	python create_symlinks.py common
	git submodule update --init --recursive

# js2-mode must be byte-compiled or it runs balls-slow.
	$(MAKE) -C common/emacs.d/vendor/js2-mode

linux: common
	python create_symlinks.py linux

darwin: common
	python create_symlinks.py darwin

# Apply custom default settings.
	sh ~/.scripts/write_defaults.sh

# Enable our login_init hook.
	launchctl load ~/Library/LaunchAgents/login_init.plist

# Disable the stackshot daemon so we free up the Command + Control + Option +
# Shift + [.,] keyboard shortcuts.
	launchctl remove com.apple.stackshot

install_linux_packages:
	grep -v "^#" linux_packages.txt | xargs sudo apt-get install -y
