#!/usr/bin/env bash

CDIR=$1

# Symlink .local/bin to scripts.
# Has to be done now, because absolute path differs from user to user
ln -sf $CDIR/scripts $CDIR/.local/bin/scripts

echo "Symlinking configurations with stow..."
which stow &> /dev/null
stow_installed=$?
if [[ 0 -ne $stow_installed ]]; then
    echo "stow not installed"
    echo "Installing stow..."
    sudo pacman -S stow --noconfirm
fi

# stow the correct files into the home directory
stow --verbose=2 --dir $CDIR --target $HOME .

