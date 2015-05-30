--
-- Custom utilities for window movement and resizing.
--
local winmove = {}

local util = require "util"

-- Maps a window ID to its frame before it was maximized. This lets us revert
-- windows back to their original frames when we unmaximize them.
local PRE_MAXIMIZED_WINDOW_FRAMES = {}

-- Helper function for initializing a winmove configuration.
function winmove.config(widthRatio, heightRatio, positions)
    return function()
        winmove.moveToNextPosition(widthRatio, heightRatio, positions)
    end
end

-- Moves the focused window to the next winmove cell.
function winmove.moveToNextPosition(widthRatio, heightRatio, positions)
    local win = hs.window.focusedWindow()
    local winFrame = win:frame()
    local screenFrame = win:screen():frame()

    -- Helper that returns the rect object corresponding to the given position
    -- table.
    local function getPositionRect(position)
        return {
            x = position[1] * screenFrame.w + screenFrame.x,
            y = position[2] * screenFrame.h + screenFrame.y,
            w = screenFrame.w * widthRatio,
            h = screenFrame.h * heightRatio
        }
    end

    local nextPositionIx = 1
    for i, position in ipairs(positions) do
        local positionRect = getPositionRect(position)

        -- If the window is already in the current position, then move it to the
        -- next one.
        if util.isRectsApproxMatch(winFrame, positionRect) then
            nextPositionIx = util.mod(i + 1, #positions)
            break
        -- If the window mostly overlaps the current target, then move it so
        -- that it's exactly in the current cell.
        elseif util.isRectsOverlap(winFrame, positionRect) then
            nextPositionIx = i
            break
        end
    end

    win:setFrame(getPositionRect(positions[nextPositionIx]))
end

-- Toggles maximizing / unmaximizing the current window.
--
-- If the current window is not maximized, then maximize it. Otherwise reset it
-- back to its position before it was maximized.
function winmove.toggleMaximized()
    local win = hs.window.focusedWindow()
    local winFrame = win:frame()
    local screenFrame = win:screen():frame()

    if util.isRectsApproxMatch(winFrame, screenFrame) then
        if PRE_MAXIMIZED_WINDOW_FRAMES[win:id()] then
            win:setFrame(PRE_MAXIMIZED_WINDOW_FRAMES[win:id()])
            PRE_MAXIMIZED_WINDOW_FRAMES[win:id()] = nil
        end
    else
        PRE_MAXIMIZED_WINDOW_FRAMES[win:id()] = winFrame
        win:maximize()
    end
end

-- Moves the current window one screen east.
function winmove.moveToNextScreen()
    hs.window.focusedWindow():moveOneScreenEast()
end

-- Moves the current window one screen west.
function winmove.moveToPrevScreen()
    hs.window.focusedWindow():moveOneScreenWest()
end

-- Pushes the window all the way to the north side of the screen.
function winmove.pushNorth()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    frame.y = win:screen():frame().y
    win:setFrame(frame)
end

-- Pushes the window all the way to the south side of the screen.
function winmove.pushSouth()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    frame.y = win:screen():frame().y + win:screen():frame().h - frame.h
    win:setFrame(frame)
end

-- Pushes the window all the way to the east side of the screen.
function winmove.pushEast()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    frame.x = win:screen():frame().x + win:screen():frame().w - frame.w
    win:setFrame(frame)
end

-- Pushes the window all the way to the west side of the screen.
function winmove.pushWest()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    frame.x = win:screen():frame().x
    win:setFrame(frame)
end

return winmove
