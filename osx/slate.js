/* ---------------- */
/* CONFIG VARIABLES */
/* ---------------- */

slate.configAll({
    "defaultToCurrentScreen": true,
    "checkDefaultsOnLoad": true,

    // When changing focus windows, keep searching for a window up to 10000px
    // away from the current window before stopping. I'm not sure why this
    // shouldn't just be something like maxint...
    "focusCheckWidthMax": 10000,

    // Bring forward all windows of an application when we switch into it.
    "switchOnlyFocusMainWindow": false,

    // Show window hints for windows that are hidden behind other windows.
    "windowHintsIgnoreHiddenWindows": false,

    // Show icons with window hints.
    "windowHintsShowIcons": true,

    // Spread window hints apart for hidden windows.
    "windowHintsSpread": true
});

/* ------------------- */
/* GENERAL KEYBINDINGS */
/* ------------------- */

slate.bindAll({
    // Change focus
    "j:cmd;ctrl;alt": slate.op("focus", {"direction": "down"}),
    "k:cmd;ctrl;alt": slate.op("focus", {"direction": "up"}),
    "h:cmd;ctrl;alt": slate.op("focus", {"direction": "left"}),
    "l:cmd;ctrl;alt": slate.op("focus", {"direction": "right"}),
    "m:cmd;ctrl;alt": slate.op("focus", {"direction": "behind"}),

    // Throw windows between screens.
    ",:cmd;ctrl;alt": slate.op("throw", {"screen": "left"}),
    ".:cmd;ctrl;alt": slate.op("throw", {"screen": "right"}),

    // Use custom window switcher.
    // "tab:alt": slate.op("switch"),

    // Show window hints.
    "space:cmd;ctrl;alt": slate.op("hint"),

    // Shell commands
    "`:cmd;ctrl;alt": slate.op("shell", {
        "command": "/usr/bin/osascript -l AppleScript open_finder.scpt",
        "wait": true,
        "path": "~/.scripts"
    }),
    "e:cmd;ctrl;alt": slate.op("shell", {
        "command": "/usr/bin/osascript -l AppleScript activate_emacs.scpt",
        "wait": true,
        "path": "~/.scripts"
    }),
    "w:cmd;ctrl;alt": slate.op("shell", {
        "command": "/usr/bin/osascript -l AppleScript open_google_chrome.scpt",
        "wait": true,
        "path": "~/.scripts"
    }),
    "return:cmd;ctrl;alt": slate.op("shell", {
        "command": "/usr/bin/osascript -l AppleScript open_iterm.scpt",
        "wait": true,
        "path": "~/.scripts"
    })
});

/* ------------------ */
/* LAYOUT KEYBINDINGS */
/* ------------------ */

// Maximize the window on the current screen.
slate.bind("y:cmd;ctrl;alt", function(win) {
    win.doOperation(
        slate.op("move", {
            "x": "screenOriginX",
            "y": "screenOriginY",
            "width": "screenSizeX",
            "height": "screenSizeY"
        })
    );
});

// Resize the window so it's 2/3 the width of the screen and throw it to the
// left or right.
slate.bind("u:cmd;ctrl;alt", function(win) {
    var screenCoords = rectToCoords(win.screen().visibleRect());
    var winCoords = rectToCoords(win.rect());

    var direction = null;

    if (isApprox(winCoords.x1, screenCoords.x1) &&
        isApprox(winCoords.y1, screenCoords.y1)) {
        direction = "right";
    } else {
        direction = "left";
    }

    win.doOperation(
        slate.op("push", {
            "direction": direction,
            "style": "bar-resize:screenSizeX*2/3"
        })
    );
});

// Resize the window so it's 1/3 the width of the screen and move it between the
// 3 columns.
slate.bind("i:cmd;ctrl;alt", function(win) {
    var screenCoords = rectToCoords(win.screen().visibleRect());
    var winCoords = rectToCoords(win.rect());

    var targetX = null;
    // If the window's at the far right of the screen, then move it to the left.
    if (isApprox(winCoords.x2, screenCoords.x2) &&
        isApprox(winCoords.y1, screenCoords.y1)) {
        targetX = "screenOriginX";
    }
    // If the window's at the far left of the screen, move it to the center.
    else if (isApprox(winCoords.x1, screenCoords.x1) &&
               isApprox(winCoords.y1, screenCoords.y1)) {
        targetX = "screenOriginX + screenSizeX/3";
    }
    // Otherwise, move it to the far right.
    else {
        targetX = "screenOriginX + screenSizeX*2/3";
    }

    win.doOperation(
        slate.op("move", {
            "x": targetX,
            "y": "screenOriginY",
            "width": "screenSizeX/3",
            "height": "screenSizeY"
        })
    );
});

// Resize the window so it's 1/3 the width of the screen and 1/2 the height of
// the screen, and move it between the four corners.
slate.bind("o:cmd;ctrl;alt", function(win) {
    var screenCoords = rectToCoords(win.screen().visibleRect());
    var winCoords = rectToCoords(win.rect());

    var direction = null;

    // If it's at the top-right corner, move it to the bottom-right.
    if (isApprox(winCoords.x2, screenCoords.x2) &&
        isApprox(winCoords.y1, screenCoords.y1)) {
        direction = "bottom-right";
    }
    // If it's at the bottom-right corner, move it to the bottom-left.
    else if (isApprox(winCoords.x2, screenCoords.x2) &&
             isApprox(winCoords.y2, screenCoords.y2)) {
        direction = "bottom-left";
    }
    // If it's at the bottom-left corner, move it to the top-left.
    else if (isApprox(winCoords.x1, screenCoords.x1) &&
             isApprox(winCoords.y2, screenCoords.y2)) {
        direction = "top-left";
    }
    // Otherwise, move the window to the top-right by default.
    else {
        direction = "top-right";
    }

    win.doOperation(
        slate.op("corner", {
            "direction": direction,
            "width": "screenSizeX/3",
            "height": "screenSizeY/2"
        })
    );
});

// Resize the window so it's 1/2 the width of the screen and throw it to the
// left or right.
slate.bind("p:cmd;ctrl;alt", function(win) {
    var screenCoords = rectToCoords(win.screen().visibleRect());
    var winCoords = rectToCoords(win.rect());

    var direction = null;

    if (isApprox(winCoords.x1, screenCoords.x1) &&
        isApprox(winCoords.y1, screenCoords.y1)) {
        direction = "right";
    } else {
        direction = "left";
    }

    win.doOperation(
        slate.op("push", {
            "direction": direction,
            "style": "bar-resize:screenSizeX/2"
        })
    );
});

/* ---------------- */
/* HELPER FUNCTIONS */
/* ---------------- */

/**
 * Converts a rect object to a coordinate objects.
 *
 * The input rect object is expected to be of the given format:
 *
 *     {
 *         "x": int,
 *         "y": int,
 *         "width": int,
 *         "height": int
 *     }
 *
 * The return value is a coord object
 *
 *     {
 *         "x1": int,
 *         "y1": int,
 *         "x2": int,
 *         "y2": int
 *     }
 *
 * where (x1, y1) is the coordinate of the top-left corner and (x2, y2) is the
 * coordinate of the bottom-right corner.
 */
function rectToCoords(rectObj) {
    return {
        "x1": rectObj.x,
        "y1": rectObj.y,
        "x2": rectObj.x + rectObj.width,
        "y2": rectObj.y + rectObj.height
    };
}

/**
 * Return true if the two value are within a certain percent of each other.
 */
function isApprox(value1, value2, tolerance) {
    tolerance = tolerance || 0.03;
    if (Math.max(value1, value2) === 0) {
        return true;
    } else if ((1 - Math.min(value1, value2) / Math.max(value1, value2)) < tolerance) {
        return true;
    } else {
        return false;
    }
};
