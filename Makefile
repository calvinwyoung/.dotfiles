.PHONY: common linux darwin install_linux_packages

common:
	python create_symlinks.py common
	git submodule update --init --recursive
	$(MAKE) -C common/emacs.d/vendor/js2-mode

linux: common
	python create_symlinks.py linux

darwin: common
	python create_symlinks.py darwin
	sh ~/.scripts/write_defaults.sh
	launchctl load ~/Library/LaunchAgents/login_init.plist

install_linux_packages:
	sudo xargs -a linux_packages.txt apt-get install
