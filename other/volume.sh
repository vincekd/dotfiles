#!/bin/bash

INCREMENT="5"
if [ ! -z "$2" ]; then
    INCREMENT="$2"
fi

if [ "$1" == "up" ]; then
    #amixer -D pulse set Master ${INCREMENT}%+ >/dev/null
    pactl set-sink-volume 0 +${INCREMENT}%
elif [ "$1" == "down" ]; then
    #amixer -D pulse set Master ${INCREMENT}%- >/dev/null
    pactl set-sink-volume 0 -${INCREMENT}%
else
    echo "invalid arg: $1"
fi
