.PHONY: common

common:
	git submodule update --init --recursive
	python create_symlinks.py common

linux: common
	python create_symlinks.py linux

darwin: common
	python create_symlinks.py darwin
	sh ~/.scripts/write_defaults.sh
	launchctl load ~/Library/LaunchAgents/login_init.plist

install_linux_packages:
	sudo xargs -a linux_packages.txt apt-get install
