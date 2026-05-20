#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name=git

: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/${name}"
bin_target="${HOME}/.local/bin"

log "Installing git..." info
install_packages git

log "Linking configuration..." info
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.config/${name}")" \
    "$config_target"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.local/bin/git-fixup")" \
    "${bin_target}/git-fixup"

log "Done." info
