#vincekd bashrc file

# drop out if not interactive shell
if [ -z "$PS1" ]; then
    return
fi


#
# various options
#
export INPUTRC=$HOME/.inputrc
export HISTCONTROL=ignoredups
export HISTIGNORE="&:??:[ ]*:clear:exit:logout:hist *"
export HISTSIZE=99999
export SAVEHIST=5000
export HISTFILE=~/.bash_history
export EDITOR='env TERM=xterm-256color em'
export DISPLAY=:0.0
export PROMPT_COMMAND='history -a'

shopt -s histverify
shopt -s checkwinsize
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s histappend
shopt -s nocaseglob


#
# aliases
#
LS_OPTS="--ignore='\.\.' --ignore='\.' --color=auto --group-directories-first --sort=time"
alias l="ls -Fa ${LS_OPTS}"
alias ll="l -lho --time-style='+%Y-%m-%d %H:%M'" # --time=ctime"
alias em="emacs -nw"
#alias hist="history | grep -E -v '^ *[0-9]+ *hist +' | grep -i"
alias hist="history | grep -i"
alias mktar="tar -cvf"
alias grep="grep --color=auto"


# set in .inputrc
# bind '"\e[1;5C":forward-word'
# #arrow left
# bind '"\e[1;5D":backward-word'
# bind '"\C-k":kill-line'
# #down arrow
# bind '"\e[B":history-search-forward'
# #down up arrow
# bind '"\e[A":history-search-backward'


#
# customize look
#
# if [[ -f ~/.DIR_COLORS ]]; then
#     echo "found colors"
#     eval "`dircolors -b ~/.DIR_COLORS`"
# fi
#export LESS_TERMCAP_us=$'\E[01;32m'
#colors
PURPLE='\e[1;35m'
CYAN='\e[0;35m'
BLUE='\e[1;34m'
YELLOW='\e[1;33m'
WHITE='\e[0;37m'
RED='\e[0;31m'

export PROMPT_DIRTRIM=4
#NAME="\[${PURPLE}\]\u\[${YELLOW}\]\[${WHITE}\]@\[${PURPLE}\]\h"
NAME="\[${PURPLE}\]\u@\h"
WDIR="\[${BLUE}\]\w"
PROMPT="\[${YELLOW}\]>"
CURCMD="\[${WHITE}\]"
PS1="${NAME} ${WDIR} \n ${PROMPT} ${CURCMD}"
#PS2='>'


#
# custom functions
#
extract () {
    if [[ $# -eq 0 ]]; then
        echo "No arguments entered"
    else
        if [[ -f $1 ]]; then
            case $1 in
                *.tar.bz2)   tar xjf $1     ;;
                *.tar.gz)    tar xzf $1     ;;
                *.bz2)       bunzip2 $1     ;;
                *.rar)       rar x $1       ;;
                *.gz)        gunzip $1      ;;
                *.tar)       tar xf $1      ;;
                *.tbz2)      tar xjf $1     ;;
                *.tgz)       tar xzf $1     ;;
                *.zip)       unzip $1       ;;
                *.Z)         uncompress $1  ;;
                *.7z)        7z x $1        ;;
                *)           echo "'$1' cannot be extracted via extract()" ;;
            esac
        else
            echo "'$1' is not a valid file"
        fi
    fi
}

dirsize () {
    du -shx * .[a-zA-Z0-9_]* 2> /dev/null | egrep '^ *[0-9.]*[MG]' \
        | sort -n > /tmp/dir-size-list
    egrep '^ *[0-9.]*M' /tmp/dir-size-list
    egrep '^ *[0-9.]*G' /tmp/dir-size-list
    rm /tmp/dir-size-list
}

backup () {
    if [[ -e $1 ]]; then
        cp $1 ${1}-`date +%Y-%m-%d_%H-%M`.backup
    else
        echo "No such file or folder"
    fi
}
