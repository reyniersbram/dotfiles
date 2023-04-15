#     _    _     ___    _    ____  _____ ____
#    / \  | |   |_ _|  / \  / ___|| ____/ ___|
#   / _ \ | |    | |  / _ \ \___ \|  _| \___ \
#  / ___ \| |___ | | / ___ \ ___) | |___ ___) |
# /_/   \_\_____|___/_/   \_\____/|_____|____/

### Simple aliases

# ls
alias ls='ls --color=auto -hF'
alias ll='ls -l'
alias la='ll -A'

# cd
alias cdd='cd ..'

# grep
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

# pacman
alias pacupdate='sudo pacman -Syu'
alias pacrefresh='sudo pacman -Syyu'
alias pacinstall='sudo pacman -S'
alias pacsearch='pacman -Ss'
alias pacuninstall='sudo pacman -Rns'

# paru
alias parupdate='paru -Sua'
alias parinstall='paru -S'
alias parsearch='paru -Ss'
alias paruninstall='paru -Rns'

# confirm overwrite
alias cp='cp -i'
alias mv='mv -i'

# Human-readable sizes
alias df='df -h'
alias free='free -m'

# use less instead of more
# More information:
# https://unix.stackexchange.com/questions/81129/what-are-the-differences-between-most-more-and-less
alias more=less

# Use neovim instead of vim
alias vim='nvim'

# Print $PATH entries on newlines
PATH () {
    echo $PATH | sed 's/:/\n/g'
}

# ex - archive extractor
# usage: ex <file>
ex () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $    ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Print usable colors for text markup in terminal.
colors() {
	local fgc bgc vals seq0

	printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}

