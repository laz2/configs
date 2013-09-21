#!/bin/bash

wmctrl -l | grep 'Skype' | grep -v 'dude.03' | cut -d' ' -f5,6,7 | xargs wmctrl -a
