-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
require("vicious")

-- Load custom modules
require("custom.layouts.tile")
require("custom.monkeypatches")
local util = require("custom.util")

-----------------
-- Error handling
-----------------

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors
    })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function(err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({
            preset = naughty.config.presets.critical,
            title = "Oops, an error happened!",
            text = err
        })
        in_error = false
    end)
end

-----------------------
-- Variable definitions
-----------------------

-- This is used later as the default terminal and editor to run.
terminal = "xterm"
editor = "emacsclient -c"
browser = "chromium-browser"

-- Set windows key as modkey
modkey = "Mod4"

-- Save the path to the config directory
config_dir = awful.util.getdir("config")

-- Themes define colours, icons, and wallpapers
beautiful.init(config_dir .. "/theme/theme.lua")

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts = {
    custom.layouts.tile,
    custom.layouts.tile.left,
    awful.layout.suit.floating,
    awful.layout.suit.max
}

-- Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({"1", "2", "3", "4"}, s, layouts[1])
end

--------
-- Wibox
--------

-- Create a menu widget and launcher
menu = awful.menu({
    items = {
        {"Restart Awesome", awesome.restart},
        {"Suspend", "sudo pm-suspend"},
        {"Restart", "sudo reboot"},
        {"Shutdown", "sudo shutdown -h now"}
    },
    width = 120
})

launcher_widget = awful.widget.launcher({
    image=image(beautiful.awesome_icon),
    menu=menu
})

-- Create a systray
systray_widget = widget({type="systray"})

-- Create a textclock widget
clock_icon = widget({type = "imagebox"})
clock_icon.image = image(config_dir .. "/icons/clock.png")
clock_widget = awful.widget.textclock({align="right"}, "%a %b %d %r", 1)

-- Create a volume widget
volume_icon = widget({type = "imagebox"})
volume_icon.image = image(config_dir .. "/icons/spkr_01.png")

volume_widget = widget({type = "textbox"})
vicious.register(
    volume_widget,
    vicious.widgets.volume,
    function(widget, args)
        -- args is a table with two values; the first value is the
        -- volume level of the selected alsa channel, and the
        -- second is a character representing the mute state of
        -- that channel.
        if args[2] == "♫" then
            return string.format("%2d%%", args[1])
        else
            return "-- "
        end
    end, 2, "Master")

volume_buttons = awful.util.table.join(
    awful.button({ }, 1, util.exec(terminal .. " -e alsamixer")),
    awful.button({ }, 3,
                 function()
                     awful.util.spawn("amixer -q set Master toggle")
                     vicious.force({volume_widget})
                 end),
    awful.button({ }, 4,
                 function()
                     awful.util.spawn("amixer -q set Master 5%+ unmute")
                     vicious.force({volume_widget})
                 end),
    awful.button({ }, 5,
                 function()
                     awful.util.spawn("amixer -q set Master 5%- unmute")
                     vicious.force({volume_widget})
                 end)
)
volume_widget:buttons(volume_buttons)
volume_icon:buttons(volume_buttons)

-- Create a memory widget
mem_icon = widget({type = "imagebox"})
mem_icon.image = image(config_dir .. "/icons/mem.png")

mem_widget = widget({type = "textbox"})
vicious.register(mem_widget, vicious.widgets.mem,
                 function (widget, args)
                     return string.format("%d%% (%.2fGB)", args[1], args[2] / 1024.0)
                 end, 2)

-- Create CPU widget
cpu_icon = widget({type = "imagebox"})
cpu_icon.image = image(config_dir .. "/icons/cpu.png")

cpu_widget = widget({type = "textbox"})
vicious.register(cpu_widget, vicious.widgets.cpu,
                 function(widget, args)
                     local core_usages = {}
                     for i = 2, #args do
                         table.insert(core_usages, string.format("%2d%%", args[i]))
                     end
                     return table.concat(core_usages, "/")
                 end, 2)

-- Create network widgets
-- The down/up widgets just exist to display data that's generated and set by a
-- separate worker widget.
net_down_icon = widget({type = "imagebox"})
net_down_widget = widget({type = "textbox"})

net_up_icon = widget({type = "imagebox"})
net_up_widget = widget({type = "textbox"})

net_worker_widget = widget({type = "textbox"})
vicious.register(
    net_worker_widget,
    vicious.widgets.net,
    function(widget, args)
        -- Figure out the currently active network interface
        local net_iface = nil
        local f = io.open("/proc/net/route")
        local content = f:read("*all")
        _, _, net_iface = string.find(content, "(%w+)%s+00000000")
        io.close(f)

        if not net_iface or not args["{" .. net_iface .. " down_kb}"] then
            net_down_icon.image = nil
            net_down_widget.text = nil

            net_up_icon.image = nil
            net_up_widget.text = nil
        else
            net_down_icon.image = image(config_dir.. "/icons/net_down_03.png")
            net_down_widget.text = string.format(
                "%4.1f k/s", args["{" .. net_iface .. " down_kb}"])

            net_up_icon.image = image(config_dir.. "/icons/net_up_03.png")
            net_up_widget.text = string.format(
                "%4.1f k/s", args["{" .. net_iface .. " up_kb}"])
        end
    end, 1)

-- Create a spacer widget
spacer = widget({type = "textbox"})
spacer.text = " "

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}

mytaglist = {}
mytaglist.buttons = awful.util.table.join(
    awful.button({ }, 1, awful.tag.viewonly),
    awful.button({ modkey }, 1, awful.client.movetotag),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, awful.client.toggletag),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
)

mytasklist = {}
mytasklist.buttons = awful.util.table.join(
    awful.button({ }, 1, function(c)
                             if c == client.focus then
                                 c.minimized = true
                             else
                                 if not c:isvisible() then
                                     awful.tag.viewonly(c:tags()[1])
                                 end
                                 -- This will also un-minimize
                                 -- the client, if needed
                                 client.focus = c
                                 c:raise()
                             end
                         end),
    awful.button({ }, 3, function()
                             if instance then
                                 instance:hide()
                                 instance = nil
                             else
                                 instance = awful.menu.clients({ width=250 })
                             end
                         end),
    awful.button({ }, 4, function()
                             awful.client.focus.byidx(1)
                             if client.focus then client.focus:raise() end
                         end),
    awful.button({ }, 5, function()
                             awful.client.focus.byidx(-1)
                             if client.focus then client.focus:raise() end
                         end)
)

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({layout = awful.widget.layout.horizontal.leftright})
    -- Create an imagebox widget which will contain an icon indicating which
    -- layout we're using.  We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(
        awful.util.table.join(
            awful.button({ }, 1, function() awful.layout.inc(layouts, 1) end),
            awful.button({ }, 3, function() awful.layout.inc(layouts, -1) end),
            awful.button({ }, 4, function() awful.layout.inc(layouts, 1) end),
            awful.button({ }, 5, function() awful.layout.inc(layouts, -1) end)))

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
        return awful.widget.tasklist.label.currenttags(c, s)
    end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({position = "top", screen = s})

    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            launcher_widget,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s], spacer,
        s == 1 and systray_widget, spacer or nil,
        clock_widget, clock_icon, spacer,
        volume_widget, volume_icon, spacer,
        mem_widget, mem_icon, spacer,
        cpu_widget, cpu_icon, spacer,
        net_up_widget, net_up_icon, spacer,
        net_down_widget, net_down_icon, spacer,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }

end

--------------------------
-- Mouse/Keyboard Bindings
--------------------------

-- Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({}, 4, awful.tag.viewnext),
    awful.button({}, 5, awful.tag.viewprev)
))

-- Key bindings
globalkeys = awful.util.table.join(
    -- Execute programs
    awful.key({ modkey,           }, "Return", util.exec(terminal)),
    awful.key({ modkey,           }, "e", util.exec(editor)),
    awful.key({ modkey,           }, "w", util.exec(browser)),
    awful.key({ modkey, "Shift"   }, "w", util.exec(browser .. " --incognito")),
    awful.key({ modkey,           }, "`", util.exec("thunar")),
    awful.key({ modkey,           }, "F12", util.exec("slock")),
    awful.key({                   }, "Print", util.exec("import " .. os.getenv("HOME") .. "/screenshot.jpg")),
    awful.key({ modkey,           }, "a", util.exec(terminal .. " -e alsamixer")),
    awful.key({                   }, "XF86AudioMute",
              function()
                  awful.util.spawn("amixer -q set Master toggle")
                  vicious.force({volume_widget})
              end),
    awful.key({                   }, "XF86AudioRaiseVolume",
              function()
                  awful.util.spawn("amixer -q set Master 5%+ unmute")
                  vicious.force({volume_widget})
              end),
    awful.key({                   }, "XF86AudioLowerVolume",
              function()
                  awful.util.spawn("amixer -q set Master 5%- unmute")
                  vicious.force({volume_widget})
              end),

    awful.key({ modkey            }, "d",
              function()
                  awful.prompt.run(
                      {prompt = "Dict: "},
                      mypromptbox[mouse.screen].widget,
                      function(word)
                          awful.util.spawn(browser .. " 'https://www.google.com/webhp\?#q=" .. word .. "&tbs=dfn:1'")
                      end)
              end),

    -- Standard navigation
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Tab",    awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function() awful.client.swap.byidx(1)      end),
    awful.key({ modkey, "Shift"   }, "k", function() awful.client.swap.byidx(-1)     end),
    awful.key({ modkey,           }, ",", function() awful.screen.focus_relative(1)  end),
    awful.key({ modkey,           }, ".", function() awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ "Mod1",           }, "Tab",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    awful.key({ modkey, "Control" }, "r", awesome.restart),

    awful.key({ modkey,           }, "l",     function() awful.tag.incmwfact(0.05)     end),
    awful.key({ modkey,           }, "h",     function() awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function() awful.tag.incnmaster(1)       end),
    awful.key({ modkey, "Shift"   }, "l",     function() awful.tag.incnmaster(-1)      end),
    awful.key({ modkey,           }, "space", function() awful.layout.inc(layouts, 1)  end),
    awful.key({ modkey, "Shift"   }, "space", function() awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "p",     function() mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function(c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "q",      function(c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                    ),
    awful.key({ modkey, "Control" }, "Return", function(c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey, "Shift"   }, ",",      function(c) awful.client.movetoscreen(c, -1) end),
    awful.key({ modkey, "Shift"   }, ".",      function(c) awful.client.movetoscreen(c, 1)  end),

    awful.key({ modkey, "Shift"   }, "r",      function(c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function(c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function(c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end),
    awful.key({ modkey, "Control" }, "m",
        function(c)
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function(c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)

--------
-- Rules
--------

awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = true,
            keys = clientkeys,
            buttons = clientbuttons,
            -- Remove gaps in between windows
            size_hints_honor = false
        }
    },
    {
        rule = {class = "Gimp"},
        properties = {floating = true}
    },
    {
        rule = {class = "Wicd-client.py"},
        properties = {floating = true}
    },
    {
        -- Make YouTube videos appear correctly in fullscreen mode
        rule = {instance = "exe"},
        properties = {floating = true}
    },
}

----------
-- Signals
----------

-- Signal function to execute when a new client appears.
client.add_signal("manage", function(c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)