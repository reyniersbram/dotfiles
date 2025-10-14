#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" >/dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

log "Installing Vim..." info

install_packages vim

ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.vimrc")" \
    "${HOME}/.vimrc"
