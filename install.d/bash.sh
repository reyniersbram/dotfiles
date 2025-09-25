#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name=bash

: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/${name}"

log "Installing Bash..." info
install_packages bash

log "Linking configuration..." info

link_bashrc=0
if [ -f "$HOME/.bashrc" ]; then
    printf "A .bashrc is already present, are you sure you want to overwrite it? [y/N] "
    read -r answer
    case "$answer" in
        [yY][eE][sS]|[yY])
            link_bashrc=1
            ;;
        *)
            ;;
    esac
fi

if [ $link_bashrc -gt 0 ]; then
    ln --symbolic --force --no-target-directory --verbose \
        "$(realpath "${current_directory}/../.bashrc")" \
        "${HOME}/.bashrc"
fi

mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.config/${name}")" \
    "$config_target"

ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.config/dircolors")" \
    "${XDG_CONFIG_HOME}/dircolors"

log "Done." info
