#!/bin/bash

function activate {
    wmctrl -l | grep emacs | grep -v Chrome | grep -v Firefox | cut -d' ' -f5,6,7 | xargs wmctrl -a
}

activate && exit 0
emacs-snapshot --maximized &
activate
