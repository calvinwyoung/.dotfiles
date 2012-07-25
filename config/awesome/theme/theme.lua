require("awful.util")

config_dir = awful.util.getdir("config")
theme_dir = config_dir .. "/theme"

theme = {}
theme.wallpaper_cmd = {}

theme.font = "Dejavu Sans Mono 8"

theme.bg_normal = "#444444"
theme.bg_focus = "#eeeeee"
theme.bg_urgent = "#ff0000ee"
theme.bg_minimize = "#303535ee"

theme.fg_normal = "#eeeeee"
theme.fg_focus = "#444444"
theme.fg_urgent = "#f7f7f7"
theme.fg_minimize = "#b9bbbb"

theme.border_width  = "1"
theme.border_normal = "#444444"
theme.border_focus  = "#eeeeee"

-- There are another variables sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- Example:
--taglist_bg_focus = #ff0000

-- Display the taglist squares
theme.taglist_squares_sel = theme_dir .. "/taglist/squaref.png"
theme.taglist_squares_unsel = theme_dir .. "/taglist/squarew.png"

-- Set the floating window icon
theme.tasklist_floating_icon = theme_dir .. "/tasklist/floating.png"

-- Variables set for theming menu
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = theme_dir .. "/submenu.png"
theme.menu_height = "15"
theme.menu_width = "100"

-- Define the image to load
theme.titlebar_close_button_normal = theme_dir .. "/titlebar/close_normal.png"
theme.titlebar_close_button_focus = theme_dir .. "/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = theme_dir .. "/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive = theme_dir .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = theme_dir .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active = theme_dir .. "/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = theme_dir .. "/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive = theme_dir .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = theme_dir .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active = theme_dir .. "/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = theme_dir .. "/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive = theme_dir .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = theme_dir .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active = theme_dir .. "/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = theme_dir .. "/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive = theme_dir .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = theme_dir .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active = theme_dir .. "/titlebar/maximized_focus_active.png"

-- You can use your own layout icons like this:
theme.layout_fairh = theme_dir .. "/layouts/fairhw.png"
theme.layout_fairv = theme_dir .. "/layouts/fairvw.png"
theme.layout_floating = theme_dir .. "/layouts/floatingw.png"
theme.layout_magnifier = theme_dir .. "/layouts/magnifierw.png"
theme.layout_max = theme_dir .. "/layouts/maxw.png"
theme.layout_fullscreen = theme_dir .. "/layouts/fullscreenw.png"
theme.layout_tilebottom = theme_dir .. "/layouts/tilebottomw.png"
theme.layout_tileleft = theme_dir .. "/layouts/tileleftw.png"
theme.layout_tile = theme_dir .. "/layouts/tilew.png"
theme.layout_tiletop = theme_dir .. "/layouts/tiletopw.png"

-- Set the awesome icon
theme.awesome_icon = theme_dir .. "/awesome-icon.png"

return theme
