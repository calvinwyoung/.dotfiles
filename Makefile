.PHONY: common

common:
	git submodule update --init --recursive
	python create_symlinks.py common

linux: common
	python create_symlinks.py linux

osx: common
	python create_symlinks.py osx
	sh ~/.scripts/write_defaults.sh
