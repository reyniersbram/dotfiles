# Enable aliases
shopt -s expand_aliases

alias ls='ls --color=auto -hF'
alias ll='ls -l'
alias la='ll -A'

alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

alias cp='cp -i'        # confirm before overwriting something
alias mv='mv -i'        # confirm before overwriting something
alias df='df -h'        # human-readable sizes
alias free='free -m'    # show sizes in MB
alias more=less
