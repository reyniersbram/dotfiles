#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name=autorandr

: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/${name}"

log "Installing autorandr..." info
install_packages autorandr


log "Linking configuration..." info
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "${current_directory}/../.config/$name" \
    "$config_target"

log "It might be necessary to change setup files to have more accurate monitor matches" hint
log "Load the wanted profile: autorandr --load <profile_name>" hint
log "Save the setup: autorandr --save <profile_name> --force" hint

log "Done." info
