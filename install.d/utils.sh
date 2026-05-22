#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

log "Installing utilities..." info

bin_target="${HOME}/.local/bin"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.local/bin/powermenu.sh")" \
    "${bin_target}/powermenu.sh"
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.local/bin/conservation_mode.sh")" \
    "${bin_target}/conservation_mode.sh"

log "Done." info
