#!/bin/bash

#
# Create symlinks that point to these configuration files from the
# expected locations
#

# the directory that contains all setup files
CONFIG_DIR="~/config"
OVERWRITE_EXISTING_FILES="YES"

# helper functions

# usage is similar to cp or ln:
#   install-config foo bar/baz
#
# creates a link to foo at bar/baz. Creates bar if not present.
# DELETES baz if present and replaces it with a link to foo.
#
# $1 the source file
# $2 the destination
function install-config()
{
    local SOURCE="$1"
    local TARGET="$2"
    if [ ! -e "$SOURCE" ] ; then
        echo "$SOURCE" does not exist
        exit 1
    fi
    local TARGET_DIR=$(dirname "$TARGET")
    if [ ! -d "$TARGET_DIR" ] ; then
        mkdir -p "$TARGET_DIR"
    fi
    if [ ! -d "$TARGET_DIR" ] ; then
        echo dir "$TARGET_DIR" does not exit and could not be created
        exit 1
    fi
    if [ -e "$TARGET" ] && [ "$OVERWRITE_EXISTING_FILES" != "YES" ] ; then
        echo "$TARGET" exists
        exit 1
    fi
    if [ -e "$TARGET" ] ; then
        echo "deleteing old $TARGET"
        rm -f "$TARGET"
    fi
    if [ -e "$TARGET" ] ; then
        echo "error deleting $TARGET"
        exit 1
    fi
    echo symlinking "$TARGET" to "$SOURCE"
    ln -s "$SOURCE" "$TARGET"
}

if [ "$OVERWRITE_EXISTING_FILES" == "YES" ] ; then
    echo "warning: may overwrite existing files. press ctrl-d to exit now"
    read || exit
fi

if [ ! -d "$CONFIG_DIR" ] ; then
    echo "CONFIG_DIR ("$CONFIG_DIR") not found"
fi

# basics
install-config "$CONFIG_DIR"/bashrc ~/.bashrc
install-config "$CONFIG_DIR"/tmux.conf ~/.tmux.conf

# editors: vim
install-config "$CONFIG_DIR"/vimrc ~/.vimrc

# editors: emacs
mkdir -p ~/.emacs.d
install-config "$CONFIG_DIR"/emacs.d/init.el ~/.emacs.d/init.el
install-config "$CONFIG_DIR"/emacs.d/snippets ~/.emacs.d/snippets

# custom folder names
install-config "$CONFIG_DIR"/user-dirs ~/.config/user-dirs.dirs

# terminals
install-config "$CONFIG_DIR"/Xresources ~/.Xresources
