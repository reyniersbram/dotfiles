#    ____    _    ____  _   _ ____   ____
#   | __ )  / \  / ___|| | | |  _ \ / ___|
#   |  _ \ / _ \ \___ \| |_| | |_) | |
#  _| |_) / ___ \ ___) |  _  |  _ <| |___
# (_)____/_/   \_\____/|_| |_|_| \_\\____|
#
# https://shreevatsa.wordpress.com/2008/03/30/zshbash-startup-files-loading-order-bashrc-zshrc-etc/

### ENV VARS
export TERM='xterm-256color'
export TERMINAL="kitty"
export HISTCONTROL="ignoredups:erasedups"
export EDITOR=nvim
export VISUAL=nvim

### XDG Base Directory Specification
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# https://wiki.archlinux.org/title/XDG_Base_Directory
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
# list of directories separated by `:`
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

export PYTHON_VENV_DIR="${XDG_DATA_HOME}/python/virtual_environments"
source "${HOME}/.local/bin/pyvenv"

### If not running interactively, don't do anything
[[ $- != *i* ]] && return

### PATH

append_to_path() {
    case ":${PATH}:" in
        *:$1:*)
            ;;
        *)
            PATH="${PATH:+${PATH}:}$1"
    esac
}

append_to_path "${HOME}/.local/bin"
append_to_path "${HOME}/.local/bin/rofi"
append_to_path "${HOME}/.ghcup/bin"
append_to_path "${HOME}/.cabal/bin"

export PATH
unset -f append_to_path

### CHANGE TITLE OF TERMINALS
PROMPT_COMMAND='echo -ne "\033]0;\uf489 : $(echo ${PWD/#$HOME/\~} | sed "s/.*\/\([^\/]\)/\1/")\007"'

### SHOPT
shopt -s cdable_vars
shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist
shopt -s expand_aliases
shopt -s histappend
shopt -s no_empty_cmd_completion

### Enable completion
[ -r /usr/share/bash-completion/bash_completion ] && source "/usr/share/bash-completion/bash_completion"
[ -r /usr/share/bash-completion/completions/git ] && source "/usr/share/bash-completion/completions/git"
bind "set completion-ignore-case on"

### source alias definitions
[ -f "${XDG_CONFIG_HOME}/bash/alias" ] && source "${XDG_CONFIG_HOME}/bash/alias"

### source prompt configuration
[ -f "${XDG_CONFIG_HOME}/bash/prompt" ] && source "${XDG_CONFIG_HOME}/bash/prompt"

# TODO: figure out what this does
xhost +local:root > /dev/null 2>&1
