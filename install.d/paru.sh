#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name="paru"
url="https://aur.archlinux.org/paru.git"
target="${repo_prefix}/${name}"
: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/$name"

mkdir --parents --verbose "${repo_prefix}"

if command -v paru > /dev/null; then
    log "paru already installed, skipping installation." warn
else
    if [ -d "$target/.git" ]; then
        log "Updating existing repo: ${target}" info
        git -C "${target}" fetch --all
        git -C "${target}" reset --hard origin/master
    else
        log "Cloning new repository to ${target}" info
        git clone "${url}" "${target}"
    fi

    log "Installing..." info
    cd "${target}"
    makepkg --syncdeps --install --clean
fi

log "Linking configuration..." info
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "${current_directory}/../.config/$name" \
    "$config_target"

log "Done." info
