.PHONY: common

common:
	git submodule update --init --recursive
	python create_symlinks.py common

linux: common
	python create_symlinks.py linux

osx: common
	python create_symlinks.py osx
	sh ~/.scripts/write_defaults.sh

install_linux_packages:
	sudo xargs -a linux_packages.txt apt-get install
