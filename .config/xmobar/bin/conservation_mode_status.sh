#!/usr/bin/env sh

path="/sys/bus/platform/drivers/ideapad_acpi/VPC2004:00/conservation_mode"
status=$(cat $path)

case "$status" in
    0)
        echo "󱈐 " # f1210
        ;;
    1)
        echo -e "󱈏 " # f120f
        ;;
    *)
        echo " " # f071
esac

echo $status
