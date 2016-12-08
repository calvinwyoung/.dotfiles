--
-- Custom utilities for window movement and resizing.
--
local clipboard = {}

local pasteboard = require "hs.pasteboard"

-- Emits keystrokes corresponding to the current contents of the clipboard.
function clipboard.pasteAsKeystrokes()
    current_contents = pasteboard.getContents()
    if (current_contents) then
        hs.eventtap.keyStrokes(current_contents)
    end
end

return clipboard
