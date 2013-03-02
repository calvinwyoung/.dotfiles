#!/bin/bash

# Start various application if they're installed on the system.
command -v thunar >/dev/null 2>&1 && { thunar --daemon & }
command -v dropbox >/dev/null 2>&1 && { dropbox start & }
command -v clipit >/dev/null 2>&1 && { clipit -n & }
command -v wicd-client >/dev/null 2>&1 && { wicd-client --tray & }

# start emacs daemon if it's not already running
if [ ! $(pidof emacs) ]
then
    emacs --daemon
fi

# Set wallpaper
eval `cat .fehbg`

# Evaluate Xresources
xrdb -merge .Xresources

# Set mouse speed and keyboard repeat rate
xset m 3.5 1 r rate 250

xset +fp ~/.fonts
xset fp rehash

# Swap caps lock and ctrl
setxkbmap -option ctrl:swapcaps

# Set mouse config
if [ -e $HOME/.Xmodmap ]; then
    xmodmap $HOME/.Xmodmap
fi

# Start the window manager!
# conky -c ~/.dwm/conkyrc | while read LINE; do xsetroot -name "$LINE"; done &
# ~/.dwm/statusbar.py | while read LINE; do xsetroot -name "$LINE"; done &
# while true; do
#     ck-launch-session dbus-launch ~/.dwm/dwm >/dev/null 2>&1
# done
exec ck-launch-session dbus-launch awesome
