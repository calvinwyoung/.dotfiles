-- Source: http://www.robpeck.com/blog/2010/05/18/scripting-iterm-with-applescript/
tell application "System Events"
    set appWasRunning to (name of processes) contains "iTerm"

    tell application "iTerm"
        activate

        if not appWasRunning then
            terminate the first session of the first terminal
        end if

        tell (make new terminal)
            launch session "Default"
        end tell
    end tell
end tell

on ApplicationIsRunning(appName)
    tell application "System Events" to set appNameIsRunning to exists (processes where name is appName)
    return appNameIsRunning
end ApplicationIsRunning

-- (*
--  * New-iTerm-Window.scpt
--  *
--  * Intended for use with QuickSilver
--  *  I mapped option-y to running this script to create
--  *  a new iTerm window on the current workspace
--  *
--  * Based on much Googling - very little "original" code here
--  *   Comments/Suggestions to brad.lhotsky@gmail.com
--  *)
-- if isAppRunning("iTerm") then
--     tell application "iTerm"
--         tell (make new terminal)
--             launch session "Default"
--         end tell
--     end tell
-- else
--     tell application "iTerm"
--         activate
--     end tell
-- end if
--
-- (* Code from Dweller on
--  *  http://codesnippets.joyent.com/posts/show/1124
--  *)
-- on isAppRunning(appName)
--     tell application "System Events" to (name of processes) contains appName
-- end isAppRunning
