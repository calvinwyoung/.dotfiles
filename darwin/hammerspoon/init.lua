local focus = require "focus"
local layouts = require "layouts"
local util = require "util"
local winmove = require "winmove"
local breaktimer = require "breaktimer"

-- Disable all animations.
hs.window.animationDuration = 0

-- Hotkey definitions
local HYPER = {"ctrl", "alt", "cmd"}
local HYPER_SHIFT = {"ctrl", "alt", "cmd", "shift"}

util.bindAll({
    -- Change window focus.
    {HYPER, "j", focus.focusNextWindow},
    {HYPER, "k", focus.focusPrevWindow},
    {HYPER_SHIFT, "0", focus.toggleCurrentWindowFocusDirection},

    -- Change screen focus.
    {HYPER, "[", focus.focusNextScreen},
    {HYPER, "]", focus.focusPrevScreen},

    -- Move windows to different screens.
    {HYPER_SHIFT, "[", winmove.moveToPrevScreen},
    {HYPER_SHIFT, "]", winmove.moveToNextScreen},

    -- Move and resize windows on the same screen.
    {
        HYPER,
        "m",
        winmove.toggleMaximized
    },
    {
        HYPER,
        "u",
        winmove.config(2/3, 1, {{0, 0}, {1/3, 0}})
    },
    {
        HYPER,
        "i",
        winmove.config(1/3, 1, {{0, 0}, {1/3, 0}, {2/3, 0}})
    },
    {
        HYPER,
        "n",
        winmove.config(1/3, 1/2, {{0, 0}, {2/3, 0}, {2/3, 1/2}, {0, 1/2}})
    },
    {
        HYPER_SHIFT,
        "u",
        winmove.config(1/2, 1, {{0, 0}, {1/2, 0}})
    },
    {
        HYPER_SHIFT,
        "n",
        winmove.config(1/2, 1/2, {{0, 0}, {1/2, 0}, {1/2, 1/2}, {0, 1/2}})
    },

    -- Apply pre-defined window layouts.
    {HYPER_SHIFT, "return", layouts.applyDualMonitorLayout},

    -- Toggle the break timer settings.
    {HYPER, "0", breaktimer.toggleTimer},

    -- Show window hints.
    {HYPER, "space", hs.hints.windowHints},

    -- Reload config.
    {
        HYPER_SHIFT,
        "r",
        function()
            hs.alert.show("Config loaded")
            hs.reload()
        end
    }
})
