#!/bin/bash

wmctrl -a $1
if [ $? -eq 0 ]; then
    exit 0
fi

$2 &
wmctrl -a $1
