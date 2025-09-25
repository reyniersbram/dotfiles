#!/usr/bin/env sh

set -eu

current_directory=$(cd -- "$(dirname -- "$0")" > /dev/null 2>&1 && pwd)
. "${current_directory}/../lib.sh"


: "${XDG_CONFIG_HOME:=$HOME/.config}"
config_target="${XDG_CONFIG_HOME}/nvim"

log "Installing Neovim..." info

install_packages nvim

printf "Do you want to install the Lua language server as well? [y/N] "
read -r answer

case "$answer" in
    [yY][eE][sS]|[yY])
        log "Installing language server..." info
        install_packages lua-language-server
        ;;
    *)
        log "Skipping installation of language server." hint
        ;;
esac

log "Linking configuration..." info
mkdir --parents --verbose "$XDG_CONFIG_HOME"
ln --symbolic --force --no-target-directory --verbose \
    "${current_directory}/../.config/nvim" \
    "$config_target"

log "Installing plugins..." info
nvim --headless "+Lazy! restore" +qa
nvim --headless -c "Lazy! clean" -c "qa"
nvim --headless -c "TSUpdateSync" -c "qa"; echo

log "Done." info
