---------------------------------------------------------------------------
-- @author Calvin Young <calvinwyoung@gmail.com>
--
-- A few monkeypatches to the default Awesome libraries.
---------------------------------------------------------------------------

local ipairs = ipairs
local awful = require("awful")

--- Utility methods for Awesome
module("custom.monkeypatches")

-- Bound the number of master windows between 1 and the number of currently
-- visible clients.
local old_incnmaster = awful.tag.incnmaster
awful.tag.incnmaster = function(i)
    local nmaster = awful.tag.getnmaster()
    if i < 0 and nmaster > 1 then
        old_incnmaster(i)
    elseif i > 0 then
        local num_clients = 0
        for i, tag in ipairs(awful.tag.selectedlist()) do
            num_clients = num_clients + #tag:clients()
        end
        if nmaster < num_clients then
            old_incnmaster(i)
        end
    end
end

-- Disable startup-notification (ie stopwatch cursor icon) globally
local old_spawn = awful.util.spawn
awful.util.spawn = function(s)
  old_spawn(s, false)
end
