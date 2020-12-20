#!/usr/bin/env bash

exec dbus-launch --exit-with-session emacs -mm --debug-init
# exec picom -b --experimental-backends --dbus --config /home/elken/.config/awesome/configuration/picom.conf
# exec /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 & eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)
