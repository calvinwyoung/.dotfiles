--
-- Helper methods for changing focus between windows and screens.
--
local focus = {}

local util = require "util"

-- Stores the ID of the last focused window on each screen. This is used to move
-- focus back to that window when we switch focus to that screen.
local LAST_FOCUSED_WIN_BY_SCREEN = {}

 -- Maps a screen ID to the window focus direction for that screen. A positive
 -- value focuses on windows clockwise.
local SCREEN_FOCUS_DIRECTIONS = {}

-- Returns a list of *all* visible windows on the current screen.
local function getVisibleWindowsCurrentScreen()
    return getVisibleWindowsByScreen(hs.screen.mainScreen():id())
end

-- Returns a list of *all* visible windows on the screen with the given ID.
local function getVisibleWindowsByScreen(screenId)
    local screenWindows = {}
    for i, win in ipairs(hs.window.visibleWindows()) do
        if win:screen():id() == screenId then
            if (win:isStandard() and win:title():len() > 0) or (win:application():title() == "HipChat") then
                table.insert(screenWindows, win)
            end
        end
    end

    return screenWindows
end

-- Gets the default window focus direction for the given screen.
function focus.getWindowFocusDirection(screenId)
    if not SCREEN_FOCUS_DIRECTIONS[screenId] then
        SCREEN_FOCUS_DIRECTIONS[screenId] = 1
    end

    return SCREEN_FOCUS_DIRECTIONS[screenId]
end

-- Sets the default window focus direction for the given screen.
function focus.setWindowFocusDirection(screenId, direction)
    SCREEN_FOCUS_DIRECTIONS[screenId] = direction
end

-- Toggles the default window focus direction for the given screen.
function focus.toggleCurrentWindowFocusDirection()
    local screenId = hs.window.focusedWindow():screen():id()
    local curFocusDirection = focus.getWindowFocusDirection(screenId)
    focus.setWindowFocusDirection(screenId, -curFocusDirection)
end

-- Focuses on the next window.
function focus.focusNextWindow(reverse)
    local win = hs.window.focusedWindow()
    if not win then
        return
    end

    local screen = win:screen()
    local screenCenter = util.getRectCenter(screen:frame())
    local screenWindows = getVisibleWindowsByScreen(screen:id())

    local direction = focus.getWindowFocusDirection(screen:id())
    if reverse then
        direction = -direction
    end

    local function winCmp(a, b)
        -- Get the center points for the two windows.
        local aCenter = util.getRectCenter(a:frame())
        local bCenter = util.getRectCenter(b:frame())

        if aCenter.x >= screenCenter.x and bCenter.x < screenCenter.x then
            return true
        elseif aCenter.x < screenCenter.x and bCenter.x >= screenCenter.x then
            return false
        elseif aCenter.x == screenCenter.x and bCenter.x == screenCenter.x then
            return aCenter.y > bCenter.y
        end

        -- Compute the cross product of vectors (center -> a) x (center -> b).
        local det = (
            (aCenter.x - screenCenter.x) * (bCenter.y - screenCenter.y) -
            (bCenter.x - screenCenter.x) * (aCenter.y - screenCenter.y));

        -- If the cross product is positive, then windows a and b form a
        -- clockwise rotation around the screen center. Here, we invert the sign
        -- so that points in the clockwise rotation get ordered first.
        local result = -det

        if result == 0 then
            -- If points a and b are on the same line from the center, check
            -- which point is closer to the screen center.
            local aDist = ((aCenter.x - screenCenter.x) ^ 2 +
                           (aCenter.y - screenCenter.y) ^ 2)
            local bDist = ((bCenter.x - screenCenter.x) ^ 2 +
                           (bCenter.y - screenCenter.y) ^ 2)

            -- If window a is farther from the center, then it should
            -- precede b.
            result = bDist - aDist
        end

        -- If the result is still 0, then order the windows based on their ID.
        if result == 0 then
            result = a:id() - b:id()
        end

        return (result * direction) < 0
    end

    table.sort(screenWindows, winCmp)

    -- It's possible that the current window is not a standard window, in which
    -- case it won't be found in the screenWindows list. In these cases, we
    -- should be sure to assign curWinIx a default value.
    local curWinIx = hs.fnutils.indexOf(screenWindows, win) or 0
    screenWindows[curWinIx % #screenWindows + 1]:focus()
end

function focus.focusPrevWindow()
    focus.focusNextWindow(true)
end

-- Focuses on the next screen.
function focus.focusNextScreen(reverse)
    local win = hs.window.focusedWindow()
    if not win then
        return
    end

    local allScreens = hs.screen.allScreens()
    local curScreenIx = hs.fnutils.indexOf(allScreens, win:screen())

    local direction = util.ternary(reverse, -1, 1)
    local nextScreenIx = util.mod(curScreenIx + direction, #allScreens)
    local nextScreenId = allScreens[nextScreenIx]:id()
    local nextScreenWindows = getVisibleWindowsByScreen(nextScreenId)

    if #nextScreenWindows > 0 then
        -- First, save the current window as the last-focused window for this
        -- screen. That way switching back to this screen will focus on this
        -- window.
        LAST_FOCUSED_WIN_BY_SCREEN[win:screen():id()] = win:id()

        local nextWindow
        if LAST_FOCUSED_WIN_BY_SCREEN[nextScreenId] then
            nextWindow = hs.fnutils.find(
                nextScreenWindows,
                function(win)
                    return win:id() == LAST_FOCUSED_WIN_BY_SCREEN[nextScreenId]
            end)
        else
            nextWindow = nextScreenWindows[1]
        end

        if nextWindow then
            nextWindow:focus()
        end
    end
end

-- Focuses on the previous screen.
function focus.focusPrevScreen()
    focus.focusNextScreen(true)
end

return focus
