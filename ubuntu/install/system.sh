#!/bin/bash

function install {
    sudo apt-get install -y $1
}

function remove {
    sudo apt-get purge $1
}

install tmux
install emacs
install krusader
install konsole
install tilda
install liferea
install cmake
install htop
install thunderbird
install firefox
install network-manager-openconnect
install workrave

remove unity-scope-video-remote
remove unity-scope-musicstores
