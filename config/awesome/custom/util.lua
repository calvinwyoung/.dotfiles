---------------------------------------------------------------------------
-- @author Calvin Young <calvinwyoung@gmail.com>
--
-- Misc. utility functions for configuring Awesome.
---------------------------------------------------------------------------
local awful = require("awful")

module("custom.util")

-- Helper function to bind commands to execute programs
function exec(cmd)
    return function() awful.util.spawn(cmd) end
end
