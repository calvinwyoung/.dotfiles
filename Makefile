.PHONY: common darwin linux clean convert_plists install_linux_packages

common:
	python create_symlinks.py common/
	git submodule update --init --recursive

# js2-mode must be byte-compiled or it runs balls-slow.
	$(MAKE) -C common/emacs.d/vendor/js2-mode

linux: common
	python create_symlinks.py linux/

darwin: common
	python create_symlinks.py darwin/

# Apply custom default settings.
	sh ~/.scripts/write_defaults.sh

# Enable our login_init hook.
	launchctl load ~/Library/LaunchAgents/login_init.plist

clean:
	python create_symlinks.py -l common/  | tr "\n" "\0" | xargs -0 rm -f
	python create_symlinks.py -l linux/ | tr "\n" "\0" | xargs -0 rm -f
	python create_symlinks.py -l darwin/ | tr "\n" "\0" | xargs -0 rm -f

convert_plists:
	find darwin -name "*.plist" | xargs plutil -convert xml1

install_linux_packages:
	grep -v "^#" linux_packages.txt | xargs sudo apt-get install -y

download_osx_packages:
	bash download_osx_packages.sh
