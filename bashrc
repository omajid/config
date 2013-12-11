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

# Set up term correctly

if [ "$TERM" = "xterm" ] ; then
    if [ -z "$COLORTERM" ] ; then
        if [ -z "$XTERM_VERSION" ] ; then
            echo "Warning: Terminal wrongly calling itself 'xterm'."
        else
            case "$XTERM_VERSION" in
            "XTerm(256)") TERM="xterm-256color" ;;
            "XTerm(88)") TERM="xterm-88color" ;;
            "XTerm") ;;
            *)
                echo "Warning: Unrecognized XTERM_VERSION: $XTERM_VERSION"
                ;;
            esac
        fi
    else
        case "$COLORTERM" in
            gnome-terminal)
                # Those crafty Gnome folks require you to check COLORTERM,
                # but don't allow you to just *favor* the setting over TERM.
                # Instead you need to compare it and perform some guesses
                # based upon the value. This is, perhaps, too simplistic.
                TERM="xterm-256color"
                ;;
            "Terminal")
               TERM="xterm-256color"
               ;;
            *)
                echo "Warning: Unrecognized COLORTERM: $COLORTERM"
                ;;
        esac
    fi
fi

# fall back sanely on misisng terminfo files
SCREEN_COLORS="`tput colors`"
if [ -z "$SCREEN_COLORS" ] ; then
    case "$TERM" in
        screen-*color-bce)
            echo "Unknown terminal $TERM. Falling back to 'screen-bce'."
            export TERM=screen-bce
            ;;
        *-88color)
            echo "Unknown terminal $TERM. Falling back to 'xterm-88color'."
            export TERM=xterm-88color
            ;;
        *-256color)
            echo "Unknown terminal $TERM. Falling back to 'xterm-256color'."
            export TERM=xterm-256color
            ;;
    esac
    SCREEN_COLORS=`tput colors`
fi
if [ -z "$SCREEN_COLORS" ] ; then
    case "$TERM" in
        gnome*|xterm*|konsole*|aterm|[Ee]term)
            echo "Unknown terminal $TERM. Falling back to 'xterm'."
            export TERM=xterm
            ;;
        rxvt*)
            echo "Unknown terminal $TERM. Falling back to 'rxvt'."
            export TERM=rxvt
            ;;
        screen*)
            echo "Unknown terminal $TERM. Falling back to 'screen'."
            export TERM=screen
            ;;
    esac
    SCREEN_COLORS=`tput colors`
fi

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

alias bat='upower -i /org/freedesktop/UPower/devices/battery_BAT0| grep -E "state|to\ full|percentage"'
