--
-- Module that allows us to draw a colored menubar to indicate some sort of
-- status.
--
local statusindicator = {}

local draw = require "hs.drawing"
local colors = draw.color.x11

-- Initialize the empty indicator table and layout.
local ind = nil

function initIndicator()
   if ind ~= nil then
      deleteIndicator()
   end
   ind = {}
end

function statusindicator.deleteIndicator()
   if ind ~= nil then
      for i,v in ipairs(ind) do
         if v ~= nil then
            v:delete()
         end
      end
      ind = nil
   end
end

function statusindicator.drawIndicatorBlue()
    drawIndicator(colors.blue)
end

function statusindicator.drawIndicatorGreen()
    drawIndicator(colors.green)
end

function statusindicator.drawIndicatorYellow()
    drawIndicator(colors.yellow)
end

function statusindicator.drawIndicatorRed()
    drawIndicator(colors.red)
end

function drawIndicator(color)
    initIndicator()

    screens = hs.screen.allScreens() -- color all screens
    for i, screen in ipairs(screens) do
        local screeng = screen:fullFrame()
        local width = screeng.w
        local height = screen:frame().y - screeng.y
        c = draw.rectangle(hs.geometry.rect(screeng.x+(width*(i-1)), screeng.y,
                                            width, height))
        c:setFillColor(color)
        c:setFill(true)
        c:setAlpha(0.4)
        c:setLevel(draw.windowLevels.overlay)
        c:setStroke(false)
        c:setBehavior(draw.windowBehaviors.canJoinAllSpaces)
        c:show()
        table.insert(ind, c)
    end
end

return statusindicator
