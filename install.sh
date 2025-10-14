#!/usr/bin/env sh

# Main install script.

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/lib.sh"

if [ $# -gt 0 ]; then
    script="${current_directory}/install.d/$1.sh"
    if [ -x "$script" ]; then
        "$script"
        exit 0
    else
        log "No install script for $1" error
        exit 1
    fi
fi

log "Running a full system upgrade before starting the installation..." info
system_upgrade

log "Installing packages required to run this install script..." info
install_packages base-devel make git

log "Creating XDG Base Directories..." info
mkdir -vp "${XDG_CONFIG_HOME}"
mkdir -vp "${XDG_CACHE_HOME}"
mkdir -vp "${XDG_DATA_HOME}"
mkdir -vp "${XDG_STATE_HOME}"

# Directories that should not be symlinked
log "Creating extra directories..." info
mkdir -vp "${HOME}/.local/bin"

# Setup system files
log "Setting up sudoers files..." info
for file in "${current_directory}/etc/sudoers/"*; do
    sudo cp --verbose "${file}" "/etc/sudoers.d/$(basename "${file}")"
done

log "Installing custom programs..." info
for script in "${current_directory}/install.d/"*; do
    [ -x "$script" ] && "$script"
done
