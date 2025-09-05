#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name=dunst

: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/${name}"

log "Installing dunst..." info
install_packages libnotify dunst

log "Linking configuration..." info
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "${current_directory}/../.config/${name}" \
    "$config_target"

log "Done." info
