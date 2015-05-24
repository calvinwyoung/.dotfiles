--
-- Apply predefined window layouts.
--
local layouts = {}

local focus = require "focus"
local patches = require "patches"

local allScreens = hs.screen.allScreens()
local dualMonitorLayout = {
    {
        "Google Chrome",
        nil,
        allScreens[1],
        hs.geometry.rect(1/3, 0, 2/3, 1),
        nil,
        nil
    },
    {
        "HipChat",
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
}

function layouts.applyDualMonitorLayout()
    focus.setWindowFocusDirection(allScreens[1]:id(), -1)
    focus.setWindowFocusDirection(allScreens[2]:id(), 1)
    patches.layout_apply(dualMonitorLayout)
end

return layouts
