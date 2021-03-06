--
-- Apply predefined window layouts.
--
local layouts = {}

local focus = require "focus"
local patches = require "patches"
local util = require "util"

-- Define constant for the name of the laptop screen.
local LAPTOP_SCREEN_NAME = "Built-in Retina Display"

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
            "Slack",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 1/2, 1/3, 1/2),
            nil,
            nil
        },
        {
            "Code",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 0, 1, 1),
            nil,
            nil
        },
        {
            "iTerm2",
            nil,
            allScreens[1],
            {hs.geometry.rect(1/2, 0, 1/2, 1/2), hs.geometry.rect(1/2, 1/2, 1/2, 1/2)},
            nil,
            nil
        },
        {
            "Code",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 0, 1, 1),
            nil,
            nil
        },
    })
end

local function applyUltrawideSingleMonitorLayout()
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
            hs.geometry.rect(0, 0, 1/2, 1),
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
            "Slack",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 1/2, 1/3, 1/2),
            nil,
            nil
        },
        {
            "iTerm2",
            nil,
            allScreens[1],
            {hs.geometry.rect(2/3, 0, 1/3, 1/2), hs.geometry.rect(2/3, 1/2, 1/3, 1/2)},
            nil,
            nil
        },
        {
            "Code",
            nil,
            allScreens[1],
            hs.geometry.rect(1/2, 0, 1/2, 1),
            nil,
            nil
        },
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
            "Slack",
            nil,
            allScreens[1],
            hs.geometry.rect(0, 1/2, 1/3, 1/2),
            nil,
            nil
        },
        {
            "Code",
            nil,
            allScreens[2],
            hs.geometry.rect(0, 0, 2/3, 1),
            nil,
            nil
        },
        {
            "iTerm2",
            nil,
            allScreens[2],
            {hs.geometry.rect(2/3, 0, 1/3, 1/2), hs.geometry.rect(2/3, 1/2, 1/3, 1/2)},
            nil,
            nil
        }
    })
end

local function applyLaptopWithMonitorLayout()
    local allScreens = hs.screen.allScreens()
    if #allScreens ~= 2 then
        return
    end

    local laptopScreen = util.find(
        allScreens,
        function(s) return s:name() == LAPTOP_SCREEN_NAME end)

    local monitorScreen = util.find(
        allScreens,
        function(s) return s:name() ~= LAPTOP_SCREEN_NAME end)

    if not laptopScreen then
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
            laptopScreen,
            hs.geometry.rect(0, 0, 1, 1),
            nil,
            nil
        },
        {
            "Messages",
            nil,
            laptopScreen,
            hs.geometry.rect(0, 0, 1/2, 1/2),
            nil,
            nil
        },
        {
            "Slack",
            nil,
            laptopScreen,
            hs.geometry.rect(0, 1/2, 1/2, 1/2),
            nil,
            nil
        },
        {
            "Code",
            nil,
            monitorScreen,
            {hs.geometry.rect(0, 0, 1, 1)},
            nil,
            nil
        },
        {
            "iTerm2",
            nil,
            laptopScreen,
            {hs.geometry.rect(1/2, 0, 1/2, 1/2), hs.geometry.rect(1/2, 1/2, 1/2, 1/2)},
            nil,
            nil
        }
    })
end

local function applyLaptopWithUltrawideMonitorLayout()
    local allScreens = hs.screen.allScreens()
    if #allScreens ~= 2 then
        return
    end

    local laptopScreen = util.find(
        allScreens,
        function(s) return s:name() == LAPTOP_SCREEN_NAME end)

    local monitorScreen = util.find(
        allScreens,
        function(s) return s:name() ~= LAPTOP_SCREEN_NAME end)

    if not laptopScreen then
        return
    end

    -- Set the proper focus direction for each screen.
    focus.setWindowFocusDirection(allScreens[1]:id(), -1)
    focus.setWindowFocusDirection(allScreens[2]:id(), 1)

    -- Move windows to the correct places.
    patches.layout_apply({
        {
            "Messages",
            nil,
            laptopScreen,
            hs.geometry.rect(0, 0, 1/2, 1/2),
            nil,
            nil
        },
        {
            "Slack",
            nil,
            laptopScreen,
            hs.geometry.rect(0, 1/2, 1/2, 1/2),
            nil,
            nil
        },
        {
            "iTerm2",
            nil,
            laptopScreen,
            {hs.geometry.rect(1/2, 0, 1/2, 1/2), hs.geometry.rect(1/2, 1/2, 1/2, 1/2)},
            nil,
            nil
        },
        {
            "Google Chrome",
            nil,
            monitorScreen,
            hs.geometry.rect(0, 0, 1/2, 1),
            nil,
            nil
        },
        {
            "Code",
            nil,
            monitorScreen,
            {hs.geometry.rect(1/2, 0, 1/2, 1)},
            nil,
            nil
        },
    })
end

function layouts.applyLayoutForScreens()
    local allScreens = hs.screen.allScreens()
    local laptopScreen = util.find(
        allScreens,
        function(s) return s:name() == LAPTOP_SCREEN_NAME end)
    local monitorScreen = util.find(
        allScreens,
        function(s) return s:name() ~= LAPTOP_SCREEN_NAME end)

    if #allScreens == 1 and allScreens[1]:frame().w > 3000 then
        applyUltrawideSingleMonitorLayout()
    elseif #allScreens == 1 then
        applySingleMonitorLayout()
    elseif #allScreens == 2 and laptopScreen and monitorScreen and monitorScreen:frame().w > 3000 then
        applyLaptopWithUltrawideMonitorLayout()
    elseif #allScreens == 2 and laptopScreen then
        applyLaptopWithMonitorLayout()
    else
        applyDualMonitorLayout()
    end
end

hs.screen.watcher.new(layouts.applyLayoutForScreens):start()

return layouts
