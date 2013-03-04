#!/bin/bash
#
# On OS X 10.8, ClipMenu consistently consumes ~30% of CPU, which is
# absurd. This script uses the ``cpulimit`` tool to prevent it from consuming
# more than 5%. This script requires the ``cpulimit`` command, which can be
# found here: https://github.com/opsengine/cpulimit

echo "RUNNING!" >> /Users/calvin/foo

while true; do
    clipmenu_pid=$(ps -A | grep -m1 ClipMenu | awk '{print $1}')

    if [ -z $clipmenu_pid ]; then
        echo "ClipMenu not found. Sleeping for 5 second and trying again."
        sleep 5
    else
        echo "Clipmenu found! Starting cpulimit on process $clipmenu_pid."
        cpulimit -l 5 -p $clipmenu_pid
    fi
done
