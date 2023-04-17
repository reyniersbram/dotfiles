#!/usr/bin/env bash

CDIR=$1

which paru &> /dev/null
paru_installed=$?
if [[ 0 -ne $paru_installed ]]; then
    echo "paru not installed"
    echo "Installing paru from AUR..."

    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si --clean
    cd ..
    rm -rf paru
else
    echo "paru already installed"
fi

