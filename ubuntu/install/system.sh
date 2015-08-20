#!/bin/bash

function install {
    sudo apt-get install -y $1
}

function remove {
    sudo apt-get purge $1
}

install tmux
install emacs
install krusader konsole
install tilda
install liferea

remove unity-lens-shopping unity-scope-video-remote unity-scope-musicstores
