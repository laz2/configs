#!/bin/bash

SCRIPT=$(readlink -f $0)
SCRIPT_ROOT_PATH=$(dirname $SCRIPT)

$SCRIPT_ROOT_PATH/run_or_active.sh RubyMine /home/laz2/tools/RubyMine-5.4.3.2.1/bin/rubymine.sh
