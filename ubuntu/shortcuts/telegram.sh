#!/bin/bash

SCRIPT=$(readlink -f $0)
SCRIPT_ROOT_PATH=$(dirname $SCRIPT)

$SCRIPT_ROOT_PATH/run_or_active.sh Telegram telegram-desktop
