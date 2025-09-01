#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name=bash

: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/${name}"

log "Installing Bash..." info
install_packages bash

link_bashrc=1
if [ -f "$HOME/.bashrc" ]; then
    printf "A .bashrc is already present, are you sure you want to overwrite it? [y/N] "
    read -r answer
    case "$answer" in
        [yY][eE][sS]|[yY])
            ;;
        *)
            link_bashrc=0
            ;;
    esac
fi

if [ $link_bashrc -gt 0 ]; then
    log "Linking .bashrc" info
    ln --symbolic --force --no-target-directory \
        "${current_directory}/../.bashrc" \
        "$HOME/.bashrc"
fi

log "Linking configuration..." info
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "${current_directory}/../.config/${name}" \
    "$config_target"

log "Done." info
