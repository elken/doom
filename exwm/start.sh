#!/usr/bin/env bash

exec >~/.logs/xsession 2>&1
export LANG="en_GB.UTF-8"
export LANGUAGE="en_GB.UTF-8"
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus
export _JAVA_AWT_WM_NONREPARENTING=1
wmname LG3D
xset -dpms
xset s off
# xss-lock -- gnome-screensaver-command -l &
xhost +SI:localuser:$USER
# picom -b --experimental-backends --dbus --config ~/.doom.d/exwm/picom.conf
emacs -mm --with-exwm
