#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

log "Installing packages for feh..." info
install_packages feh

WALLPAPER_DIR="$HOME/pictures/wallpapers"
mkdir --verbose --parents "$HOME/pictures"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../pictures/wallpapers")" \
    "$WALLPAPER_DIR"

ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.fehbg")" \
    "$HOME/.fehbg"
