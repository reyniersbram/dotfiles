#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

log "Installing packages for X11..." info
install_packages xorg-server xorg-xinit xorg-xrdb xorg-xrandr

log "Linking configuration..." info
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.xinitrc")" \
    "$HOME/.xinitrc"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.Xresources")" \
    "$HOME/.Xresources"
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.config/Xresources")" \
    "${XDG_CONFIG_HOME}/Xresources"
