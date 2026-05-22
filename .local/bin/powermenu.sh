#!/usr/bin/env sh

DMENU="dmenu -i -l 7 -p Power:"

# stored as key|value
choices="\
suspend|󰤄 Suspend
suspend_hibernate|󰤆 Suspend then Hibernate
hibernate|󰋊 Hibernate
reboot|󰜉 Reboot
shutdown|󰤂 Shutdown
conservation|󱈑 Toggle Conservation Mode
lock|󰷛 Lock screen"

# get choice
choice=$(printf "%s\n" "$choices" | cut -d'|' -f2 | $DMENU)

# map choice back to key
key=$(printf "%s\n" "$choices" | awk -F'|' -v c="$choice" '$2==c {print $1}')

case "$key" in
    suspend)
        systemctl suspend
        ;;
    suspend_hibernate)
        systemctl suspend-then-hibernate
        ;;
    hibernate)
        systemctl hibernate
        ;;
    reboot)
        systemctl reboot
        ;;
    shutdown)
        systemctl poweroff
        ;;
    conservation)
        conservation_mode.sh toggle
        ;;
    lock)
        slock
        ;;
    *)
        exit 0
        ;;
esac
