# start misc application
urxvtd -q -o -f
thunar --daemon &
dropbox start &
# wicd-client &
trayer --transparent true --alpha 255 --edge bottom --align right --widthtype request &

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

# swap caps lock and ctrl
setxkbmap -option ctrl:swapcaps

# set mouse config
if [ -e $HOME/.Xmodmap ]; then
    xmodmap $HOME/.Xmodmap
fi

conky -c ~/.dwm/conkyrc | while read LINE; do xsetroot -name "$LINE"; done &
exec ck-launch-session dbus-launch ~/.dwm/dwm >/dev/null 2>&1
# exec ck-launch-session dbus-launch openbox
