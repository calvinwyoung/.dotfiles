/* See LICENSE file for copyright and license details. */

/* appearance */
#define NUMCOLORS 5
static const char colors[NUMCOLORS][ColLast][8] = {
    // border   foreground background
    { "#444444", "#ffffff", "#141414" },  // normal
    { "#bdef34", "#141414", "#bdef34" },  // selected
    { "#ff0000", "#000000", "#ffff00" },  // urgent/warning  (black on yellow)
    { "#ff0000", "#ffffff", "#ff0000" },  // error (white on red)
    { "#141414", "#bdef34", "#141414" },  // status titles
};
static const char font[]            = "-*-dejavu sans mono-medium-r-*-*-10-*-*-*-*-*-*-*";
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */

/* tagging */
static const char *tags[] = { "1", "2", "3", "4" };

static const Rule rules[] = {
    /* class      instance    title       tags mask     isfloating   monitor */
    { "Gimp",     NULL,       NULL,       0,            True,        -1 },
    { "Quodlibet", NULL,      NULL,       1 << 1,       False,       -1 },
    { "trayer",   NULL,       NULL,       1 << 3,       False,       -1 },
};

/* layout(s) */
static const float mfact      = 0.55; /* factor of master area size [0.05..0.95] */
static const Bool resizehints = False; /* True means respect size hints in tiled resizals */
static const int layoutaxis[] = {
    1,    /* layout axis: 1 = x, 2 = y */
    1,    /* master axis: 1 = x (l/r), 2 = y (t/b), 3 = z (monocle) */
    2,    /* stack axis:  1 = x (l/r), 2 = y (t/b), 3 = z (monocle) */
};

/* include */
#include "layouts/flextile.h"
#include "layouts/gaplessgrid.c"
#include "layouts/fibonacci.c"
#include "layouts/push.c"
#include <X11/XF86keysym.h>

static const Layout layouts[] = {
    /* symbol     arrange function */
    { "[T]",      tile },
    { "[D]",      dwindle },
    { "[M]",      monocle },
    { "[G]",      gaplessgrid },
    { "[F]",      NULL },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *dmenu[] = { "dmenu_run", "-fn", font, "-nb", colors[0][ColBG], "-nf", colors[0][ColFG], "-sb", colors[1][ColBG], "-sf", colors[1][ColFG], NULL };
static const char *terminal[]  = { "urxvt", NULL };
static const char *emacs[]  = { "emacsclient", "-c", NULL };
static const char *webbrowser[]  = { "chromium-browser", NULL };
static const char *filemanager[]  = { "thunar", NULL };
static const char *alsamixer[]  = { "urxvt", "-e", "alsamixer", NULL };
static const char *quodlibet[]  = { "quodlibet", NULL };
static const char *displayoff[]  = { "displayoff", NULL };
static const char *shutdown[]  = { "sudo", "shutdown", "-h", "now", NULL };
static const char *wicdclient[]  = { "wicd-client", NULL };
static const char *raisevolume[]  = { "amixer", "-q", "set", "Master", "5%+", "unmute", NULL };
static const char *lowervolume[]  = { "amixer", "-q", "set", "Master", "5%-", "unmute", NULL };
static const char *mutevolume[]  = { "amixer", "-q", "set", "Master", "toggle", NULL };
static const char *mpdplay[]  = { "ncmpcpp", "toggle", NULL };
static const char *mpdstop[]  = { "ncmpcpp", "stop", NULL };
static const char *mpdprev[]  = { "ncmpcpp", "prev", NULL };
static const char *mpdnext[]  = { "ncmpcpp", "next", NULL };

static Key keys[] = {
    /* modifier                     key        function        argument */
    /* applications */
    { MODKEY,                       XK_Return, spawn,          {.v = terminal } },
    { MODKEY,                       XK_grave,  spawn,          {.v = filemanager } },
    { MODKEY,                       XK_p,      spawn,          {.v = dmenu } },
    { MODKEY,                       XK_e,      spawn,          {.v = emacs } },
    { MODKEY,                       XK_w,      spawn,          {.v = webbrowser } },
    { MODKEY,                       XK_a,      spawn,          {.v = alsamixer } },
    { MODKEY,                       XK_q,      spawn,          {.v = quodlibet } },
    { MODKEY,                       XK_n,      spawn,          {.v = wicdclient } },
    { MODKEY,                       XK_F12,    spawn,          {.v = displayoff } },
    { MODKEY|ControlMask,           XK_F12,    spawn,          {.v = shutdown } },

    /* media */
    { 0,           XF86XK_AudioRaiseVolume,    spawn,          {.v = raisevolume } },
    { 0,           XF86XK_AudioLowerVolume,    spawn,          {.v = lowervolume } },
    { 0,           XF86XK_AudioMute,           spawn,          {.v = mutevolume  } },
    { 0,           XF86XK_AudioPlay,           spawn,          {.v = mpdplay  } },
    { 0,           XF86XK_AudioStop,           spawn,          {.v = mpdstop  } },
    { 0,           XF86XK_AudioPrev,           spawn,          {.v = mpdprev  } },
    { 0,           XF86XK_AudioNext,           spawn,          {.v = mpdnext  } },

    { MODKEY,                       XK_b,      togglebar,      {0} },
    { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
    { Mod1Mask,                     XK_Tab,    focusstack,     {.i = +1 } },
    { Mod1Mask|ShiftMask,           XK_Tab,    focusstack,     {.i = -1 } },
    { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY|ControlMask,           XK_Return, zoom,           {0} },
    { MODKEY,                       XK_Tab,    view,           {0} },
    { MODKEY|ControlMask,           XK_q,      killclient,     {0} },
    { MODKEY|ControlMask,           XK_BackSpace, quit,           {0} },

    { MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
    { MODKEY,                       XK_d,      setlayout,      {.v = &layouts[1]} },
    { MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
    { MODKEY,                       XK_g,      setlayout,      {.v = &layouts[3]} },
    { MODKEY,                       XK_f,      setlayout,      {.v = &layouts[4]} },
    { MODKEY,                       XK_space,  setlayout,      {0} },
    { MODKEY|ControlMask,           XK_space,  togglefloating, {0} },
    { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
    { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
    { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
    { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
    { MODKEY|ShiftMask,             XK_j,      pushdown,       {0} },
    { MODKEY|ShiftMask,             XK_k,      pushup,         {0} },

    { MODKEY|ControlMask|ShiftMask, XK_space,  rotatelayoutaxis, {.i = 0} }, /* layout axis */
    { MODKEY|ControlMask,           XK_m,      rotatelayoutaxis, {.i = 1} }, /* master axis */
    { MODKEY|ShiftMask,             XK_m,      rotatelayoutaxis, {.i = 2} }, /* stack axis */
    { MODKEY|ShiftMask,             XK_Return, mirrorlayout,     {0} },
    { MODKEY|ControlMask,           XK_k,      shiftmastersplit, {.i = -1} }, /* reduce the number of tiled clients in the master area */
    { MODKEY|ControlMask,           XK_j,      shiftmastersplit, {.i = +1} }, /* increase the number of tiled clients in the master area */

    /* window navigations */
    TAGKEYS(                        XK_1,                      0)
    TAGKEYS(                        XK_2,                      1)
    TAGKEYS(                        XK_3,                      2)
    TAGKEYS(                        XK_4,                      3)
    TAGKEYS(                        XK_5,                      4)
    TAGKEYS(                        XK_6,                      5)
    TAGKEYS(                        XK_7,                      6)
    TAGKEYS(                        XK_8,                      7)
    TAGKEYS(                        XK_9,                      8)
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = terminal } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

