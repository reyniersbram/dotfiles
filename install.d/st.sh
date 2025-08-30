#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"

name="st"
url="git@github.com:reyniersbram/${name}.git"
upstream_url="https://git.suckless.org/st"
target="${repo_prefix}/${name}"

mkdir --parents --verbose "${repo_prefix}"

if [ -d "$target/.git" ]; then
    log "Updating existing repo: ${target}" warn
    git -C "${target}" fetch --all
    git -C "${target}" reset --hard origin/main
else
    log "Cloning new repository to ${target}" warn
    git clone "${url}" "${target}"
    git -C "${target}" remote add upstream "${upstream_url}"
fi

log "Installing..." info
cd "${target}"
sudo make clean install
log "Done." info
