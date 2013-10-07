#!/bin/bash

SCRIPT=$(readlink -f $0)
SCRIPT_ROOT_PATH=$(dirname $SCRIPT)

$SCRIPT_ROOT_PATH/run_or_active.sh "IntelliJ IDEA" /home/laz2/tools/idea-IU-129.1328/bin/idea.sh
