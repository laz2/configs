#!/bin/bash

SCRIPT=$(readlink -f $0)
SCRIPT_ROOT_PATH=$(dirname $SCRIPT)

ln -s $SCRIPT_ROOT_PATH/.profile ~/.profile
ln -s $SCRIPT_ROOT_PATH/.bashrc ~/.bashrc
ln -s $SCRIPT_ROOT_PATH/.bash_aliases ~/.bash_aliases
ln -s $SCRIPT_ROOT_PATH/.bash_logout ~/.bash_logout