#!/bin/bash

SCRIPT=$(readlink -f $0)
SCRIPT_ROOT_PATH=$(dirname $SCRIPT)

ln -f -s $SCRIPT_ROOT_PATH/.profile ~/.profile
ln -f -s $SCRIPT_ROOT_PATH/.bashrc ~/.bashrc
ln -f -s $SCRIPT_ROOT_PATH/.bash_aliases ~/.bash_aliases
ln -f -s $SCRIPT_ROOT_PATH/.bash_logout ~/.bash_logout
ln -f -s $SCRIPT_ROOT_PATH/.tmux.conf ~/.tmux.conf
ln -f -s $SCRIPT_ROOT_PATH/.gitconfig ~/.gitconfig
