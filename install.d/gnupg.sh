#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

log "Installing GnuPG..." info
install_packages gnupg

log "Linking configuration..." info
ln --symbolic --force --no-target-directory --verbose \
    "$(realpath "${current_directory}/../.gnupg/gpg-agent.conf")" \
    "${HOME}/.gnupg/gpg-agent.conf"

log "Done." info
