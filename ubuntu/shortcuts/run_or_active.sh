#!/bin/bash

name=$1

wmctrl -a $name
if [ $? -eq 0 ]; then
    exit 0
fi

shift
$@ &
wmctrl -a $name
