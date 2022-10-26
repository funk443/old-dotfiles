#!/bin/bash

xrandr --output DisplayPort-0 --off --output DisplayPort-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DisplayPort-2 --off --output HDMI-A-0 --off
pipewire &
pipewire-pulse &
pipewire-media-session &
picom -CGb --experimental-backends
ibus-daemon -drxR
xfce4-clipman &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
xinput set-button-map 8 1 2 3 4 5 6 7 8 2 10 11 12
numlockx
discord &
emacs --daemon 
