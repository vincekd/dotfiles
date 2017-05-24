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
# Path additions
#
if [ -d ~/scripts/ ]; then
    PATH=$PATH:~/scripts/
fi

if [ -d ~/bin/ ]; then
    PATH=$PATH:~/bin/
fi

export PATH


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
alias q="exit"
alias lss="ll | grep -i"
alias cdl="cd -"
alias gs="git status"

if [[ "$OSTYPE" == "cygwin" ]]; then
    alias emo="/cygdrive/c/Program\ Files/emacs-25.2/bin/runemacs.exe"
    alias mysql="/cygdrive/c/Program\ Files\ \(x86\)/MySQL/MySQL\ Server\ 5.6/bin/mysql.exe"
else
    alias emo="emacs"
fi

# set in keybindings in ~/.inputrc


#
# customize look
#

# Set LS_COLORS
if [ -f ~/.DIR_COLORS ]; then
    eval "`dircolors -b ~/.DIR_COLORS`"
fi

# colors
PURPLE='\e[1;35m'
CYAN='\e[0;36m'
BLUE='\e[1;34m'
YELLOW='\e[1;33m'
WHITE='\e[0;37m'
RED='\e[0;31m'
#NOCOL="\e]0;"

# prompt
export PROMPT_DIRTRIM=4
NAME="\[${PURPLE}\]\u@\h"
WINTITLE="\[\e]0;\u@\h: \w\a\]"
EXTRA="\[${YELLOW}\][\@]"
WDIR="\[${BLUE}\]\w"
PROMPT="\[${YELLOW}\]>"
CURCMD="\[${WHITE}\]"
PS1="${WINTITLE} ${NAME} ${EXTRA} ${WDIR} \n ${PROMPT} ${CURCMD}"
PS2='>'

# window title (set in PS1)
#echo -ne "\e]0;V's Bash Shell\a"

# less colors
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

#
# custom functions
#
extract () {
    if [ $# -eq 0 ]; then
        echo "No arguments entered"
    else
        if [ -f $1 ]; then
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
    du -shx * .[a-zA-Z0-9_]* 2> /dev/null | egrep '^ *[0-9.]*[MG]' | \
        sort -n > /tmp/dir-size-list
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

#go to dir
gg () {
    if [ $# -ne 1 ]; then
        echo "Wrong number of arguments"
    else
        case $1 in
            d) cd ~/dev/ ;;
            rw) cd ~/dev/rena-web/ ;;
            wp) cd ~/dev/workpro/ ;;
            dl) cd ~/Downloads/ ;;
            ad) cd ~/AppData/Roaming/ ;;
            *) echo "'$1' is not registered" ;;
        esac
    fi
}

