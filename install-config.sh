#!/bin/bash

#
# Create symlinks that point to these configuration files from the
# expected locations
#

set -euo pipefail

# the directory that contains all setup files
CONFIG_DIR="$HOME/config"
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
        echo "warning: $SOURCE" does not exist
        return
    fi
    local TARGET_DIR
    TARGET_DIR=$(dirname "$TARGET")
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
        echo "error: unable to delete $TARGET"
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
    echo "CONFIG_DIR (\"$CONFIG_DIR\") not found"
fi

# basics
install-config "$CONFIG_DIR"/bashrc ~/.bashrc
install-config "$CONFIG_DIR"/dir-colors ~/.dir_colors
install-config "$CONFIG_DIR"/tmux.conf ~/.tmux.conf

# X
install-config "$CONFIG_DIR"/x-modmap ~/.Xmodmap

# editors: vim
mkdir -p ~/.vim
install-config "$CONFIG_DIR"/vim.d/vimrc ~/.vimrc
install-config "$CONFIG_DIR"/vim.d/vim-colors-solarized/colors/solarized.vim ~/.vim/colors/solarized.vim

# editors: emacs
mkdir -p ~/.emacs.d
install-config "$CONFIG_DIR"/emacs.d/init.el ~/.emacs.d/init.el
install-config "$CONFIG_DIR"/emacs.d/snippets ~/.emacs.d/snippets

# custom folder names
install-config "$CONFIG_DIR"/user-dirs ~/.config/user-dirs.dirs

# terminals
install-config "$CONFIG_DIR"/Xresources ~/.Xresources

# mutt
mkdir -p ~/.mutt
install-config "$CONFIG_DIR"/mutt.d/muttrc ~/.mutt/muttrc
install-config "$CONFIG_DIR"/mutt.d/color-solarized-dark ~/.mutt/color-solarized-dark
install-config "$CONFIG_DIR"/mutt.d/mailcap ~/.mutt/mailcap

# irssi
mkdir -p ~/.irssi
install-config "$CONFIG_DIR"/irssi.d/irssi-colors-solarized/solarized-universal.theme ~/.irssi/solarized-universal.theme
install-config "$CONFIG_DIR"/irssi.d/config ~/.irssi/config

# awesome
mkdir -p ~/.config/awesome
install-config "$CONFIG_DIR"/awesome.d/rc.lua ~/.config/awesome/rc.lua
install-config "$CONFIG_DIR"/awesome.d/awesome-solarized ~/.config/awesome/themes/awesome-solarized

# firefox
install-config "$CONFIG_DIR"/vimperatorrc ~/.vimperatorrc

# dictionary
install-config "$CONFIG_DIR"/dictionary ~/.hunspell_en_US
# gdb
install-config "$CONFIG_DIR"/gdbinit ~/.gdbinit

