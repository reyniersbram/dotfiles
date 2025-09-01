# lib.sh - shared functions and variables for used in install scripts
# This file should be sourced at the beginning of the script

# shellcheck disable=SC2034
XDG_CONFIG_HOME="${HOME}/.config"
XDG_CACHE_HOME="${HOME}/.cache"
XDG_DATA_HOME="${HOME}/.local/share"
XDG_STATE_HOME="${HOME}/.local/state"

color_red="\033[0;31m"
color_green="\033[0;32m"
color_yellow="\033[0;33m"
color_blue="\033[0;34m"
color_reset="\033[0m"

# shellcheck disable=SC2034
repo_prefix="$HOME/repos"

# log [text] [level]
# level: hint, info, warn, error
log() {
    color=$color_reset
    case "$2" in
        hint)
            color=$color_blue
            ;;
        info)
            color=$color_green
            ;;
        warn)
            color=$color_yellow
            ;;
        error)
            color=$color_red
            ;;
    esac
    printf "%b%s%b\n" "${color}" "$1" "${color_reset}" >&2
}

system_upgrade() {
    if command -v pacman > /dev/null; then
        sudo pacman --sync --refresh --refresh --sysupgrade
    else
        log "No supported package manager found." error
        exit 1
    fi
}

install_packages() {
    if command -v pacman > /dev/null; then
        sudo pacman --sync --needed "$@"
    else
        log "No supported package manager found." error
        exit 1
    fi
}
