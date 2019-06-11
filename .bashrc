# vincekd bashrc file

# drop out if not interactive shell
if [ -z "$PS1" ]; then
    return
fi

# shopts
shopt -s histverify
shopt -s checkwinsize
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s histappend
shopt -s nocaseglob

if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi

    # git clone https://github.com/vincekd/git-completion.bash
    if [ -f ~/dev/git-completion.bash/git-completion.bash ]; then
        . ~/dev/git-completion.bash/git-completion.bash
    fi
fi


#
# shell variables
#
export INPUTRC=$HOME/.inputrc
export HISTCONTROL=ignoredups
export HISTIGNORE="&:??:[ ]*:clear:exit:logout:hist *"
export HISTSIZE=99999
export SAVEHIST=5000
export HISTFILE=~/.bash_history
if [[ "$OSTYPE" == "gnu-linux" ]]; then
    export EDITOR="emacsclient -t"
    export VISUAL="emacsclient -c"
else
    export EDITOR="emacs -nw"
    export VISUAL="emacs"
fi
export DISPLAY=:0.0
export PROMPT_COMMAND="history -a"
export GTAGSCONF=~/.globalrc
export GTAGSLABEL=pygments

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export NODE_PATH=~/.npm-global/lib/node_modules/:/usr/lib/node_modules/:/usr/lib/nodejs/
export GOPATH=~/go
export GUROBI_HOME=/opt/gurobi810/linux64
export LD_LIBRARY_PATH=$GUROBI_HOME/lib

case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac


#
# Path additions
#
if [ -d ~/scripts/ ]; then
    PATH=$PATH:~/scripts/
fi

if [ -d ~/.npm-global ]; then
    PATH=$PATH:~/.npm-global/bin
fi

if [ -d ~/bin/ ]; then
    PATH=$PATH:~/bin/
fi

if [ -d ~/dev/go_appengine ]; then
    PATH=$PATH:~/dev/go_appengine/
fi

if [ -d "$GUROBI_HOME" ]; then
    PATH=$PATH:$GUROBI_HOME/bin
fi

export PATH


#
# aliases
#
export LS_OPTS="--ignore='\.\.' --ignore='\.' --color=auto --group-directories-first --sort=time"
alias l="ls -Fa ${LS_OPTS}"
alias ll="l -lho --time-style='+%Y-%m-%d %H:%M'" # --time=ctime"
#alias em="emacs -nw"
alias em="emacsclient -t --alternate-editor='emacs -nw'"
#alias emax="emacsclient"
#alias hist="history | grep -E -v '^ *[0-9]+ *hist +' | grep -i"
alias hist="history | grep -i"
alias mktar="tar -cvf"
alias grep="grep --color=auto"
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias pss="ps aux | grep -v grep | grep -i"
alias q="exit"
alias lss="ll | grep -i"
alias cdl="cd -"
alias gs="git status"
alias gd="git diff --ignore-space-change"
alias gc="git commit -a -m "
alias gittop="git rev-parse --show-toplevel"
alias tailf="tail -f"
alias wwwtest="ping -c 3 google.com"

if [[ "$OSTYPE" == "cygwin" ]]; then
    alias emo="/cygdrive/c/emacs/bin/runemacs.exe"
else
    emo () {
        if [ ! -z "$1" ]; then
            emacsclient -c --alternate-editor="emacs" $1 1>/dev/null &
        else
            emacsclient -c --alternate-editor="emacs" 1>/dev/null &
        fi
    } >/dev/null 2>&1
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# keybindings set in ~/.inputrc

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
GREEN="\e[0;32m"
#NOCOL="\e]0;"

# prompt
export PROMPT_DIRTRIM=4
NAME="\[${PURPLE}\]\u@\h"
WINTITLE="\[\e]0;\u@\h: \w\a\]"
EXTRA="\[${YELLOW}\][\@]"
WDIR="\[${BLUE}\]\w"
PROMPT="\[${YELLOW}\]>"
CURCMD="\[${WHITE}\]"
parse_git_branch () {
    GITBRANCH=$(git branch 2> /dev/null | grep "*" | sed 's/\* *\(.*\)/\1/')
    if [ ! -z "$GITBRANCH" ]; then
        GITBRANCH="(${GITBRANCH})"
    fi
    echo "$GITBRANCH"
}

export PS1="${WINTITLE} ${NAME} ${EXTRA} ${WDIR} ${GREEN}\$(parse_git_branch)\n ${PROMPT} ${CURCMD}"
export PS2='>'

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
                *.rar)       unrar x $1     ;;
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
            n) cd ~/notes/ ;;
            # rw) cd ~/dev/rena-web/ ;;
            # wp) cd ~/dev/workpro/ ;;
            dl) cd ~/Downloads/ ;;
            df) cd ~/.dotfiles/ ;;
            # lo) cd ~/dev/lem-opt/ ;;
            # lm) cd ~/dev/lem-maps/ ;;
            *) echo "'$1' is not a registered location" ;;
        esac
    fi
}

del () {
    if [ $# -lt 1 ]
    then
        echo "Wrong number of arguments"
    else
        TRASH_DIR=$(realpath ~/.local/share/Trash)
        DATE=$(date -Iseconds | sed 's/-[^-]*$//')
        for file in "$@"
        do
            path=$(realpath "$file")
            if [[ -f "$path" || -d  "$path" ]] && [ -w "$path" ]
            then
                name=$(basename "$path" | sed 's/ /_/g')
                echo "$name"
                ext=""
                if [[ "$name" =~ "." ]]
                then
                    ext="${name##*.}"
                fi
                pname="${name%.*}"
                count=0
                while [ -e "$TRASH_DIR/files/$name" ]
                do
                    count=$((count+1))
                    name="$pname.$count"
                    if [ ! -z "$ext" ]
                    then
                        name="$name.$ext"
                    fi
                done
                outfile="$TRASH_DIR/info/$name.trashinfo"
                echo """[Trash Info]
Path=$path
DeletionDate=$DATE""" > $outfile
                mv "$path" "$TRASH_DIR/files/$name"
            else
                echo "File does not exist: '$path'"
            fi
        done
    fi
}

sfiles () {
    if [ $# -lt 1 ]
    then
        echo "Oops: need search term and optional file extension argument 'ff something groovy'"
    else
        if [ ! -z "$2" ]
        then
            ag -lQ "$1" -G "\.$2\$"
        else
            ag -lQ "$1"
        fi
    fi
}

ffiles () {
    if [ $# -lt 1 ]
    then
        echo "Need search term argument: '*file'"
    else
        format="(%TD+%Tr)\t%Y\t%P\n"
        if [ -z "$2" ]
        then
            find ~/ -iname "$1" -printf "$format" -prune
        else
            find $2 -iname "$1" -printf "$format" -prune
        fi
    fi
}


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/vince/dev/google-cloud-sdk/path.bash.inc' ]; then
    source '/home/vince/dev/google-cloud-sdk/path.bash.inc';
fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/vince/dev/google-cloud-sdk/completion.bash.inc' ]; then
    source '/home/vince/dev/google-cloud-sdk/completion.bash.inc';
fi
