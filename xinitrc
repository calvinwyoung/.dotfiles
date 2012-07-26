# start misc application
thunar --daemon &
dropbox start &
clipit -n &
command -v wicd-client >/dev/null 2>&1 && { wicd-client --tray & }

# start emacs daemon if it's not already running
if [ ! $(pidof emacs) ]
then
    emacs --daemon
fi

# set wallpaper
eval `cat .fehbg`

# evaluate Xresources
xrdb -merge .Xresources

# set mouse speed and keyboard repeat rate
xset m 3.5 1 r rate 250

xset +fp ~/.fonts
xset fp rehash

# swap caps lock and ctrl
setxkbmap -option ctrl:swapcaps

# set mouse config
if [ -e $HOME/.Xmodmap ]; then
    xmodmap $HOME/.Xmodmap
fi

# conky -c ~/.dwm/conkyrc | while read LINE; do xsetroot -name "$LINE"; done &
# ~/.dwm/statusbar.py | while read LINE; do xsetroot -name "$LINE"; done &
# while true; do
#     ck-launch-session dbus-launch ~/.dwm/dwm >/dev/null 2>&1
# done
exec ck-launch-session dbus-launch awesome
