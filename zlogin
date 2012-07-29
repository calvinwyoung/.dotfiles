if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx 2>&1 | tee /tmp/startx.log
fi
