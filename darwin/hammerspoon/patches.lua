--
-- Patched versions of built-in functions. These functions live here until a
-- patched version with the features we want make their way into master.
--
local patches = {}

local geometry = require("hs.geometry")
local appfinder = require("hs.appfinder")
local fnutils = require("hs.fnutils")
local screen = require("hs.screen")
local window = require("hs.window")


--- Custom tweaks to hs.layout.apply(table)
--- Function
--- Applies a layout to applications/windows
---
--- Parameters:
---  * table - A table describing your desired layout. Each element in the table should be another table describing a set of windows to match, and their desired size/position. The fields in each of these tables are:
---   * A string containing an application name or nil
---   * A string containing a window title or nil
---   * A string containing a screen name, or an `hs.screen` object, or a function that accepts no parameters and returns an `hs.screen` object, or nil to select the first available screen
---   * A Unit rect (see `hs.window.moveToUnit()`)
---   * A Frame rect (see `hs.screen:frame()`)
---   * A Full-frame rect (see `hs.screen:fullFrame()`)
---
--- Returns:
---  * None
---
--- Notes:
---  * If the application name argument is nil, window titles will be matched regardless of which app they belong to
---  * If the window title argument is nil, all windows of the specified application will be matched
---  * You can specify both application name and window title if you want to match only one window of a particular application.
---  * If you specify neither application name or window title, no windows will be matched :)
---  * Monitor name is a string, as found in `hs.screen:name()`. You can also pass an `hs.screen` object, or a function that returns an `hs.screen` object. If you pass nil, the first screen will be selected.
---  * The final three arguments use `hs.geometry.rect()` objects to describe the desired position and size of matched windows:
---    * Unit rect will be passed to `hs.window.moveToUnit()`
---    * Frame rect will be passed to `hs.window.setFrame()` (including menubar and dock)
---    * Full-frame rect will be passed to `hs.window.setFrame()` (ignoring menubar and dock)
---  * If either the x or y components of frame/full-frame rect are negative, they will be applied as offsets against the opposite edge of the screen (e.g. If x is -100 then the left edge of the window will be 100 pixels from the right edge of the screen).
---  * Only one of the rect arguments will apply to any matched windows. If you specify more than one, the first will win.
---  * An example usage:
---
---     ```layout1 = {
---         {"Mail", nil, "Color LCD", hs.layout.maximized, nil, nil},
---         {"Safari", nil, "Thunderbolt Display", hs.layout.maximized, nil, nil},
---         {"iTunes", "iTunes", "Color LCD", hs.layout.maximized, nil, nil},
---         {"iTunes", "MiniPlayer", "Color LCD", nil, nil, hs.geometry.rect(0, -48, 400, 48)},
---       }```
function patches.layout_apply(layout)
-- Layout parameter should be a table where each row takes the form of:
--  {"App name", "Window name","Display Name"/"hs.screen object", "unitrect", "framerect", "fullframerect"},
--  First three items in each row are strings (although the display name can also be an hs.screen object, or nil)
--  Second three items are rects that specify the position of the window. The first one that is
--   not nil, wins.
--  unitrect is a rect passed to window:moveToUnit()
--  framerect is a rect passed to window:setFrame()
--      If either the x or y components of framerect are negative, they will be applied as
--      offsets from the width or height of screen:frame(), respectively
--  fullframerect is a rect passed to window:setFrame()
--      If either the x or y components of fullframerect are negative, they will be applied
--      as offsets from the width or height of screen:fullFrame(), respectively
    for n,_row in pairs(layout) do
        local app = nil
        local wins = nil
        local display = nil
        local displaypoint = nil
        local unit = _row[4]
        local frame = _row[5]
        local fullframe = _row[6]
        local windows = nil

        -- Find the application's object, if wanted
        if _row[1] then
            app = appfinder.appFromName(_row[1])
            if not app then
                print("Unable to find app: " .. _row[1])
            end
        end

        -- Find the destination display, if wanted
        if _row[3] then
            if type(_row[3]) == "string" then
                local displays = fnutils.filter(screen.allScreens(), function(screen) return screen:name() == _row[3] end)
                if displays then
                    -- TODO: This is bogus, multiple identical monitors will be impossible to lay out
                    display = displays[1]
                end
            elseif type(_row[3]) == "function" then
                display = _row[3]()
            elseif hs.fnutils.contains(hs.screen.allScreens(), _row[3]) then
                display = _row[3]
            end
        else
            display = screen.allScreens()[1]
        end

        if not display then
            print("Unable to find display: " .. _row[3])
        else
            displaypoint = geometry.point(display:frame().x, display:frame().y)
        end

        -- Find the matching windows, if any
        if _row[2] then
            if app then
                wins = fnutils.filter(app:allWindows(), function(win) return win:title() == _row[2] end)
            else
                wins = fnutils.filter(window:allWindows(), function(win) return win:title() == _row[2] end)
            end
        elseif app then
            wins = app:allWindows()
        end

        -- Apply the display/frame positions requested, if any
        if not wins then
            print(_row[1],_row[2])
            print("No windows matched, skipping.")
        else
            for m,_win in pairs(wins) do
                local winframe = nil
                local screenrect = nil

                -- Move window to destination display, if wanted
                if display and displaypoint then
                    _win:setTopLeft(displaypoint)
                end

                -- Apply supplied position, if any
                if unit then
                    if not unit[1] then
                        _win:moveToUnit(unit)
                    elseif m <= #unit then
                        _win:moveToUnit(unit[m])
                    end
                elseif frame then
                    winframe = frame
                    screenrect = _win:screen():frame()
                elseif fullframe then
                    winframe = fullframe
                    screenrect = _win:screen():fullFrame()
                end

                if winframe then
                    if winframe.x < 0 or winframe.y < 0 then
                        if winframe.x < 0 then
                            winframe.x = screenrect.w + winframe.x
                        end
                        if winframe.y < 0 then
                            winframe.y = screenrect.h + winframe.y
                        end
                    end
                    _win:setFrame(winframe)
                end
            end
        end
    end
end

return patches
