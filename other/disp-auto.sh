#!/bin/bash

echo "monitor change: $(date)" >> /home/vince/logs/system.log

# if [ "$1" != "forked" ]; then
#     exit
# fi

# export DISPLAY=:0
# export XAUTHORITY=/home/vince/.Xauthority

# # Find out the device path to our graphics card:
# cardPath=/sys/$(udevadm info -q path -n /dev/dri/card0)

# # Detect if the monitor is connected and, if so, the monitor's ID:
# conHdmi=$(xrandr | sed -n '/HDMI1 connected/p')
# shaHdmi=$(sha1sum $cardPath/card0-HDMI-A-1/edid | cut -f1 -d " ")

# echo "$conHdmi"
# echo "$shaHdmi"

# # The useful part: check what the connection status is, and run some other commands
# if [ -n "$conHdmi" ]; then
#     if [ "$shaHdmi" = "xxxxxxxxxxxxxxxx" ]; then # Office PC
#         xrandr --output eDP1 --auto --output HDMI1 --auto --left-of eDP1
#         else  # Probably a projector
#         xrandr --output eDP1 --auto --output HDMI1 --auto --same-as eDP1
#     fi
# else
#     xrandr --output eDP1 --auto --output HDMI1 --off
# fi

status=$(cat /sys/class/drm/card0-HDMI-A-1/status)
if [[ $status = "connected" ]]; then
    echo "Connected HDMI" >> /home/vince/logs/system.log
    xrandr --output eDP-1 --auto --output HDMI-1 --auto --left-of eDP-1
    # set as same
    #xrandr --output eDP1 --auto --output HDMI1 --auto --same-as eDP1
else
    echo "Disconnected HDMI" >> /home/vince/logs/system.log
    xrandr --output eDP-1 --auto --output HDMI-1 --off
fi
