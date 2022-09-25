#!/bin/bash

picom -CGb --experimental-backends
ibus-daemon -drxR
xfce4-clipman &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
xinput set-button-map 8 1 2 3 4 5 6 7 8 2 10 11 12
numlockx
discord &
emacs --daemon 
pulseaudio &
