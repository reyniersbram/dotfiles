#! /usr/bin/env sh

export XAUTHORITY=/home/reyniersbram/.Xauthority
export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/1000/bus"

notify-send "Relax your eyes. Take a look outside for a minute." ""\
    -i "${HOME}/Pictures/icons/glasses/glasses.svg"\
    -h string:x-dunst-stack-tag:break-reminder\
    --app-name "Rest your eyes"
