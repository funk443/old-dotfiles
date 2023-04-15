#!/bin/bash

xrandr --output DisplayPort-0 --off --output DisplayPort-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DisplayPort-2 --off --output HDMI-A-0 --off
# xrandr --output DisplayPort-0 --off --output DisplayPort-1 --primary --mode 2560x1440 --pos 0x0 --rotate normal --output DisplayPort-2 --off --output HDMI-A-0 --off
# xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VGA1 --off --output HDMI1 --off --output DP1 --off --output HDMI2 --off --output DP2 --off
pipewire &
pipewire-pulse &
wireplumber &
picom -CGb
ibus-daemon -drxR
xfce4-clipman &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
/usr/libexec/polkit-gnome-authentication-agent-1 &
xinput set-button-map 8 1 2 3 4 5 6 7 8 2 10 11 12
numlockx
discord &
emacs --daemon &
