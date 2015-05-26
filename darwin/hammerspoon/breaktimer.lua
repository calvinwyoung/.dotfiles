--
-- Timer module that reminds me to take a break. Dims the screen for 20 seconds
-- every 10 minutes.
--
local breaktimer = {}

breaktimer.WORK_TIME = 10 * 60
breaktimer.BREAK_TIME = 20

-- A list of drawings that cover up each screen during a break period.
local BREAK_GUARDS = nil

-- The timer message to show on the screen during a break period.
local BREAK_TIMER_MESSAGE = nil

-- When we start the break timer, we'll initialize a `hs.timer` object in this
-- variable to take care of the actual timing mechanism.
local BREAK_TIMER = nil

-- The break timer works as a state machine that transitions between "work" mode
-- and "break" mode.
local CURRENT_STATE = nil

-- The time we made the last state transition (i.e., either moving from "work"
-- to "break" or vice versa).
local LAST_TRANSITION_TIME = nil

-- A menubar icon to indicate that the break timer is currently disabled.
local MENUBAR_ICON = nil

local function removeBreakGuards()
    if not BREAK_GUARDS then
        return
    end

    for i, guard in ipairs(BREAK_GUARDS) do
        guard:delete()
    end
    BREAK_GUARDS = nil
end

local function showBreakGuards()
    removeBreakGuards()

    BREAK_GUARDS = {}
    for i, screen in ipairs(hs.screen.allScreens()) do
        local guard = hs.drawing.rectangle(screen:fullFrame())
        guard:setFill(true)
        guard:setFillColor({red = .2, green = .2, blue = .2, alpha = 0.92})
        guard:show()

        table.insert(BREAK_GUARDS, guard)
    end
end

local function removeBreakTimerMessage()
    if not BREAK_TIMER_MESSAGE then
        return
    end

    BREAK_TIMER_MESSAGE:delete()
    BREAK_TIMER_MESSAGE = nil
end

local function showBreakTimerMessage()
    local screenFrame = hs.screen.mainScreen():fullFrame()

    BREAK_TIMER_MESSAGE = hs.drawing.text({
        x = screenFrame.w / 2 - 70,
        y = screenFrame.h / 2.3 - 60,
        w = 140,
        h = 120
    }, breaktimer.BREAK_TIME)
    BREAK_TIMER_MESSAGE:setTextFont("Helvetica Neue UltraLight")
    BREAK_TIMER_MESSAGE:setTextSize(120)
    BREAK_TIMER_MESSAGE:setTextColor({red = 1, green = 1, blue = 1, alpha = 1})
    BREAK_TIMER_MESSAGE:show()
end

local function updateBreakTimerMessage()
    BREAK_TIMER_MESSAGE:setText(
        LAST_TRANSITION_TIME + breaktimer.BREAK_TIME - os.time())
end

local function startBreakTime()
    CURRENT_STATE = "break"
    LAST_TRANSITION_TIME = os.time()

    showBreakGuards()
    showBreakTimerMessage()
end

local function startWorkTime()
    CURRENT_STATE = "work"
    LAST_TRANSITION_TIME = os.time()

    removeBreakGuards()
    removeBreakTimerMessage()
end

-- Function to be executed each time the timer interval elapses. This function
-- should simple check whether it's time to show the break guard.
local function tick()
    if CURRENT_STATE == "work" then
        -- If we've exceeded our work time, then transition into break mode.
        if os.time() - LAST_TRANSITION_TIME >= breaktimer.WORK_TIME then
            startBreakTime()
        end
    else
        if os.time() - LAST_TRANSITION_TIME >= breaktimer.BREAK_TIME then
            startWorkTime()
        -- If we're already in break mode and it's not time to transition back
        -- to work mode, then update the timer message to display the remaining
        -- break time.
        else
            updateBreakTimerMessage()
        end
    end
end

-- Enable the break timer.
function breaktimer.enable()
    CURRENT_STATE = "work"
    LAST_TRANSITION_TIME = os.time()

    BREAK_TIMER = hs.timer.new(1, tick)
    BREAK_TIMER:start()

    if MENUBAR_ICON then
        MENUBAR_ICON:delete()
    end
    MENUBAR_ICON = nil
end

-- Disable the break timer.
function breaktimer.disable()
    CURRENT_STATE = nil
    LAST_TRANSITION_TIME = nil

    BREAK_TIMER:stop()
    BREAK_TIMER = nil

    MENUBAR_ICON = hs.menubar.new()
    MENUBAR_ICON:setIcon("caffeine-on.pdf")
end

-- Returns `true` if the break timer is currently enabled.
function breaktimer.isEnabled()
    return BREAK_TIMER ~= nil
end

-- Magical function that lets us toggle the timer in different ways.
--
-- If the timer is disabled, then calling this enables the timer. If the timer
-- is in work mode, then calling this disables the timer. If the timer is in
-- break mode, then calling this skips the break and moves immediately to work
-- mode.
function breaktimer.toggleTimer()
    if not breaktimer.isEnabled() then
        breaktimer.enable()
    elseif CURRENT_STATE == "work" then
        breaktimer.disable()
    else
        startWorkTime()
    end
end

-- We should enable the break timer as soon as this module is loaded.
breaktimer.enable()

-- We should also automatically start work time when we detect that the display
-- or system wakes up.
local caffeinateWatcher = hs.caffeinate.watcher.new(function(event)
    if (event == hs.caffeinate.watcher.screensDidWake or
        event == hs.caffeinate.watcher.systemDidWake) then
        if breaktimer.isEnabled() then
            startWorkTime()
        end
    end
end)
caffeinateWatcher:start()

return breaktimer
