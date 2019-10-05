#!/bin/bash

update_brightness() {
    if [[ ! -z "$1" ]]
    then
        for i in /sys/class/backlight/intel_backlight*/brightness; do
            #echo "$1 > $i"
            echo "$1" > $i
        done
    else
        echo "No power setting supplied"
    fi
}

if [[ "$1" == "true" ]]
then
    update_brightness 7500
else
    update_brightness 750
fi
