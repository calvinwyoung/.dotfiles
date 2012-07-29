if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx 2>&1 | sudo tee /var/log/startx.log
fi
