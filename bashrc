# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

##############################
#
# bash basics
#
##############################

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=500000
shopt -s histappend
export EDITOR=vim

#############################
#
# Set up terminal and colours
#
#############################

#############################
#
# Prompt
#
#############################

OLD_PS1="$PS1"
normal="0m"
escape="\033["
grey="30m"
bold_grey="1;30m"
blue="34m"
bold_blue="1;34m"
green="32m"
bold_green="1;32m"
red="31m"
bold_red="1;31m"
#PS1="\[$escape$bold_grey[$escape$normal$escape$green\u$escape$normal@\h $escape$red\W$escape$bold_grey]$escape$normal\\$ "
PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

##############################
#
# aliases
#
##############################

alias grin='grep -rin'

function mkcd {
    mkdir -p "$1"
    cd "$1"
}

# if this is an ssh session, use a firefox instance on the local machine
if [ -n "$SSH_CONNECTION" -a -n "$DISPLAY" ]
then
    alias firefox='firefox -no-remote'
fi

##############################
#
# catch-all
#
##############################


# User specific aliases and functions

export JAVAWS_DEBUG_ARGS="-J-Xdebug -J-Xnoagent -J-Xrunjdwp:transport=dt_socket,address=8787,server=y,suspend=y -Xnofork"
export JAVA_DEBUG_ARGS="-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=y"

ulimit -c 100000000

export LIBVIRT_DEFAULT_URI='qemu:///system'

alias bat='upower -i /org/freedesktop/UPower/devices/battery_BAT0| grep -E "state| time\ to\ |percentage"'
