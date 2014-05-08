#!/bin/sh
service lightdm stop
echo DIS > /sys/kernel/debug/vgaswitcheroo/switch
echo OFF > /sys/kernel/debug/vgaswitcheroo/switch
service lightdm start
exit
