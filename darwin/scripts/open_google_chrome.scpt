tell application "Google Chrome"
     if (count of windows) is 0 or front window is not visible then
        activate
    else
        make new window
    end if
end tell
