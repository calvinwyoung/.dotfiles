--
-- Apply predefined window layouts.
--
local layouts = {}

local focus = require "focus"
local patches = require "patches"

local function applySingleMonitorLayout()
    local allScreens = hs.screen.allScreens()
    if #allScreens ~= 1 then
        return
    end

    -- Set the proper focus direction the screen
    focus.setWindowFocusDirection(allScreens[1]:id(), 1)

    -- Move windows to the correct places.
    patches.layout_apply({
        {
            "Google Chrome",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 0, 1, 1),
            nil,
            nil
        },
        {
            "Messages",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 0, 1/3, 1/2),
            nil,
            nil
        },
        {
            "Emacs",
            nil,
            allScreens[1],
            {hs.geometry.rect(0, 0, 1/2, 1), hs.geometry.rect(1/2, 0, 1/2, 1)},
            nil,
            nil
        },
        {
            "iTerm",
            nil,
            allScreens[1],
            {hs.geometry.rect(1/2, 0, 1/2, 1/2), hs.geometry.rect(1/2, 1/2, 1/2, 1/2)},
            nil,
            nil
        }
    })
end

local function applyDualMonitorLayout()
    local allScreens = hs.screen.allScreens()
    if #allScreens ~= 2 then
        return
    end

    -- Set the proper focus direction for each screen.
    focus.setWindowFocusDirection(allScreens[1]:id(), -1)
    focus.setWindowFocusDirection(allScreens[2]:id(), 1)

    -- Move windows to the correct places.
    patches.layout_apply({
        {
            "Google Chrome",
            nil,
            allScreens[1],
            hs.geometry.rect(1/3, 0, 2/3, 1),
            nil,
            nil
        },
        {
            "Messages",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 0, 1/3, 1/2),
            nil,
            nil
        },
        {
            "Emacs",
            nil,
            allScreens[2],
            {hs.geometry.rect(0, 0, 1/3, 1), hs.geometry.rect(1/3, 0, 1/3, 1)},
            nil,
            nil
        },
        {
            "iTerm",
            nil,
            allScreens[2],
            {hs.geometry.rect(2/3, 0, 1/3, 1/2), hs.geometry.rect(2/3, 1/2, 1/3, 1/2)},
            nil,
            nil
        }
    })
end

function layouts.applyLayoutForScreens()
    local numScreens = #hs.screen.allScreens()
    if numScreens == 1 then
        applySingleMonitorLayout()
    else
        applyDualMonitorLayout()
    end
end

hs.screen.watcher.new(layouts.applyLayoutForScreens):start()

return layouts
