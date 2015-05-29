#!/usr/bin/osascript
#
# Quits OSX applications that don't have any open windows.
#
# Usage: ./quit-windowless-apps.scpt
#
# To run this periodically, just add a crontab entry:
#     0 * * * * /path/to/script.scpt > /dev/null 2>&3

# List of apps that should remain open, even if we don't find any windows for
# them.
set whiteList to {"Finder", "Path Finder", "KeePassX", "Emacs-x86_64-10_9", "iTerm", "HipChat", "Google Chrome", "Eclipse", "Xcode", "iTunes"}

# Buffer to store names of all windowless apps before calling `killall` on them.
set windowlessAppNames to {}

tell application "System Events"
    repeat with theApplication in application processes
        # Skip applications that run exclusively in the background.
        if background only of theApplication is false then
            tell theApplication
                set applicationName to name
                set numWindows to count of windows
            end tell

            if numWindows is 0 and applicationName is not in whiteList then
                set end of windowlessAppNames to applicationName
            end if
        end if
    end repeat

    if count of windowlessAppNames is greater than 0 then
        # Use `killall` command to quit applications b/c for some reason sending
        # the `quit` message doesn't work here.
        set killCommand to "killall"
        repeat with windowlessAppName in windowlessAppNames
            # Be sure to quote app names in case they contain spaces.
            set killCommand to killCommand & " \"" & windowlessAppName & "\""
        end repeat
        do shell script killCommand
    end if
end tell
