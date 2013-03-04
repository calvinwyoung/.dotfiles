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
    "space:cmd;ctrl;alt": slate.op("hint")
});

/* ----------------------- */
/* APPLESCRIPT KEYBINDINGS */
/* ----------------------- */

bind_applescript(hyper_key("`"), "open_finder.scpt");
bind_applescript(hyper_key("e"), "open_emacs.scpt");
bind_applescript(hyper_key("w"), "open_google_chrome.scpt");
bind_applescript(hyper_key("return"), "open_iterm.scpt");

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

    // If the window isn't the right size, then start it at the left of the
    // screen.
    if (!(isApprox(win.rect().width, win.screen().visibleRect().width * 2 / 3) &&
          isApprox(win.rect().height, win.screen().visibleRect().height))) {
        direction = "left";
    }
    else if (isApprox(winCoords.x1, screenCoords.x1) &&
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
    // If the window isn't the right size, then start it at the left of the
    // screen.
    if (!(isApprox(win.rect().width, win.screen().visibleRect().width / 3) &&
          isApprox(win.rect().height, win.screen().visibleRect().height))) {
        targetX = "screenOriginX";
    }
    // If the window's at the far right of the screen, then move it to the left.
    else if (isApprox(winCoords.x2, screenCoords.x2) &&
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

    // If the window isn't the right size, then start it at the top-right of the
    // screen.
    if (!(isApprox(win.rect().width, win.screen().visibleRect().width / 3) &&
          isApprox(win.rect().height, win.screen().visibleRect().height / 2))) {
        direction = "top-right";
    }
    // If it's at the top-right corner, move it to the bottom-right.
    else if (isApprox(winCoords.x2, screenCoords.x2) &&
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
slate.bind("u:shift;cmd;ctrl;alt", function(win) {
    var screenCoords = rectToCoords(win.screen().visibleRect());
    var winCoords = rectToCoords(win.rect());

    var direction = null;

    if (!(isApprox(win.rect().width, win.screen().visibleRect().width / 2) &&
          isApprox(win.rect().height, win.screen().visibleRect().height))) {
        direction = "left";
    }
    else if (isApprox(winCoords.x1, screenCoords.x1) &&
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


// Resize the window so it's 1/2 the width of the screen and 1/2 the height of
// the screen, and move it between the four corners.
slate.bind("o:shift;cmd;ctrl;alt", function(win) {
    var screenCoords = rectToCoords(win.screen().visibleRect());
    var winCoords = rectToCoords(win.rect());

    var direction = null;

    // If the window isn't the right size, then start it at the top-right of the
    // screen.
    if (!(isApprox(win.rect().width, win.screen().visibleRect().width / 2) &&
          isApprox(win.rect().height, win.screen().visibleRect().height / 2))) {
        direction = "top-right";
    }
    // If it's at the top-right corner, move it to the bottom-right.
    else if (isApprox(winCoords.x2, screenCoords.x2) &&
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
            "width": "screenSizeX/2",
            "height": "screenSizeY/2"
        })
    );
});

/* ---------------- */
/* HELPER FUNCTIONS */
/* ---------------- */

/**
 * Returns a keyboard shortcut string representing the given key pressed with
 * the HYPER key.
 */
function hyper_key(key) {
    return key + ":cmd;ctrl;alt";
}

/**
 * Binds a keyboard shortcut to execute the specified applescript name. All
 * scripts are assumed to be relative to the ~/.scripts directory.
 */
function bind_applescript(shortcut, script_name) {
    slate.bind(shortcut, slate.op("shell", {
        "command": "/usr/bin/osascript " + script_name,
        "wait": true,
        "path": "~/.scripts"
    }));
}

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
 * Return true if the two value are within a certain range of each other.
 *
 * The default tolerance is set to 50 units. Here, a unit corresponds to a
 * pixel.
 */
function isApprox(value1, value2, tolerance) {
    tolerance = tolerance || 50;
    return Math.max(value1, value2) - Math.min(value1, value2) < tolerance;
};
