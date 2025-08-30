#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/lib.sh"

log "Running a full system upgrade before starting the installation..." info
system_upgrade

log "Installing packages required to run this install script..." info
install_packages base-devel make git stow

# Create directories I want
XDG_CONFIG_HOME="${HOME}/.config"
XDG_CACHE_HOME="${HOME}/.cache"
XDG_DATA_HOME="${HOME}/.local/share"
XDG_STATE_HOME="${HOME}/.local/state"

log "Creating XDG Base Directories..." info
mkdir -vp "${XDG_CONFIG_HOME}"
mkdir -vp "${XDG_CACHE_HOME}"
mkdir -vp "${XDG_DATA_HOME}"
mkdir -vp "${XDG_STATE_HOME}"

# Directories that should not be symlinked
log "Creating extra directories..." info
mkdir -vp "${HOME}/.local/bin"
mkdir -vp "${HOME}/Pictures"
mkdir -vp "${HOME}/Pictures/Screenshots"

# Use stow to symlink all configuration files, scripts, etc.
log "Symlinking files with stow..." info
stow --verbose=2 --restow --dir "${current_directory}" --target "${HOME}" .

# Install additional packages
log "Installing additional packages..." info
packages="${current_directory}/etc/packages.txt"
if [ -s "$packages" ]; then
    # shellcheck disable=SC2046
    install_packages $(xargs < "$packages")
else
    log "Nothing to do" hint
fi

log "Installing custom programs..." info
for script in "${current_directory}/install.d/"*; do
    [ -x "$script" ] && "$script"
done

# Setup system files
log "Setting up sudoers files..." info
for file in "${current_directory}/etc/sudoers/"*; do
    sudo cp --verbose "${file}" "/etc/sudoers.d/$(basename "${file}")"
done
