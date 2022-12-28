#! /bin/bash

# Enable aliases
shopt -s expand_aliases

alias ls='ls --color=auto'
alias ll='ls -alF'

alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

alias cp='cp -i'        # confirm before overwriting something
alias df='df -h'        # human-readable sizes
alias free='free -m'    # show sizes in MB
alias more=less
