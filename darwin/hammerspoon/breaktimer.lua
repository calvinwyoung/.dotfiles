--
-- Timer module that reminds me to take a break.
--
local breaktimer = {}

-- Define a list of break times. If two or more breaks would occur at the same
-- time, then the break that is defined first takes precedence.
breaktimer.BREAK_TIMINGS = {
    -- After every 10 minutes. take a 20 second break.
    {10 * 60, 20},

    -- After every 1 minute, take a 2 second break.
    {60, 2}
}

-- This will contain a list of last transition times for each break type in the
-- BREAK_TIMINGS list.
local LAST_TRANSITION_TIMES = nil

-- A list of drawings that cover up each screen during a break period.
local BREAK_GUARDS = nil

-- The timer message to show on the screen during a break period.
local BREAK_TIMER_MESSAGE = nil

-- When we start the break timer, we'll initialize a `hs.timer` object in this
-- variable to take care of the actual timing mechanism.
local BREAK_TIMER = nil

-- The break timer works as a state machine that transitions between a "work"
-- state and one of the "break" states. If we're in a "work" state (the default
-- state), then `CURRENT_STATE` will have a value of `nil`. If we're in one of
-- the "break" states, then the value of `CURRENT_STATE` will be the index of
-- the current break we're in.
local CURRENT_STATE = nil

-- A menubar icon to indicate that the break timer is currently disabled.
local MENUBAR_ICON = nil

local function removeBreakGuards()
    if not BREAK_GUARDS then
        return
    end

    for i, guard in ipairs(BREAK_GUARDS) do
        -- Need to check that the guard exists. The guard might no longer exist
        -- if we, for example, disconnected a monitor while the break guards
        -- were up.
        if guard then
            guard:delete()
        end
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
    }, tostring(breaktimer.BREAK_TIMINGS[CURRENT_STATE][2]))
    BREAK_TIMER_MESSAGE:setTextFont("Helvetica Neue UltraLight")
    BREAK_TIMER_MESSAGE:setTextSize(120)
    BREAK_TIMER_MESSAGE:setTextColor({red = 1, green = 1, blue = 1, alpha = 1})
    BREAK_TIMER_MESSAGE:show()
end

local function updateBreakTimerMessage()
    local lastTransitionTime = LAST_TRANSITION_TIMES[CURRENT_STATE]
    local breakTime = breaktimer.BREAK_TIMINGS[CURRENT_STATE][2]
    BREAK_TIMER_MESSAGE:setText(
        string.format("%2d", lastTransitionTime + breakTime - os.time()))
end

-- Start the break timer for the given break type. The `breakIx` is an index
-- into the `BREAK_TIMINGS` array.
local function startBreakTime(breakIx)
    CURRENT_STATE = breakIx

    -- When we start a break, we need to update the LAST_TRANSITION_TIMES entry
    -- for that particular break, as well as all breaks that are defined after
    -- that index. This allows us to ensure that if two breaks were to occur at
    -- the same time, then the break that is defined first takes precedence and
    -- overrides any subsequently defined breaks.
    for i=breakIx, #LAST_TRANSITION_TIMES do
        LAST_TRANSITION_TIMES[i] = os.time()
    end

    showBreakGuards()
    showBreakTimerMessage()
end

local function startWorkTime()
    for i=CURRENT_STATE, #LAST_TRANSITION_TIMES do
        LAST_TRANSITION_TIMES[i] = os.time()
    end

    CURRENT_STATE = nil

    removeBreakGuards()
    removeBreakTimerMessage()
end

-- Function to be executed each time the timer interval elapses. This function
-- should simple check whether it's time to show the break guard.
local function tick()
    if CURRENT_STATE == nil then
        -- If we've exceeded one of the work times, then we need to transition
        -- into break mode.
        for i=1, #breaktimer.BREAK_TIMINGS do
            local lastTransitionTime = LAST_TRANSITION_TIMES[i]
            local workTime = breaktimer.BREAK_TIMINGS[i][1]

            -- If we've exceeded our work time, then transition into break mode.
            if os.time() - lastTransitionTime >= workTime then
                startBreakTime(i)
                break
            end
        end
    else
        local lastTransitionTime = LAST_TRANSITION_TIMES[CURRENT_STATE]
        local breakTime = breaktimer.BREAK_TIMINGS[CURRENT_STATE][2]

        if os.time() - lastTransitionTime >= breakTime then
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
    CURRENT_STATE = nil

    LAST_TRANSITION_TIMES = {}
    for i=1, #breaktimer.BREAK_TIMINGS do
        LAST_TRANSITION_TIMES[i] = os.time()
    end

    BREAK_TIMER = hs.timer.new(1, tick)
    BREAK_TIMER:start()

    if MENUBAR_ICON then
        MENUBAR_ICON:delete()
    end
    MENUBAR_ICON = nil
end

-- Disable the break timer.
function breaktimer.disable()
    LAST_TRANSITION_TIMES = nil

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
    elseif CURRENT_STATE == nil then
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
