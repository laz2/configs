# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export CFG_ROOT=$HOME/cfg
export UBUNTU_CFG_ROOT=$CFG_ROOT/ubuntu
export HOME_CFG_ROOT=$UBUNTU_CFG_ROOT/home

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# add to PATH scripts from git configs repository
if [ -d "$UBUNTU_CFG_ROOT/bin" ]; then
   PATH="$UBUNTU_CFG_ROOT/bin:$PATH"
fi

export SUDO_ASKPASS=/usr/bin/gksudo

if [ -f "$HOME/.profile_private" ]; then
    chmod u=rw,g=,o= $HOME/.profile_private
    . "$HOME/.profile_private"	
fi

export EDITOR=/usr/bin/nano

PATH="$HOME/tools/sbt/bin:$PATH"
