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
    "n:cmd;ctrl;alt": slate.op("focus", {"direction": "behind"}),

    // Throw windows between screens.
    ",:shift;cmd;ctrl;alt": slate.op("throw", {"screen": "left"}),
    ".:shift;cmd;ctrl;alt": slate.op("throw", {"screen": "right"}),

    // Undo the last movement operation
    "/:cmd;ctrl;alt": slate.op("undo"),

    // Undo the last movement operation
    "r:shift;cmd;ctrl;alt": slate.op("relaunch"),

    // Show window hints.
    "space:cmd;ctrl;alt": slate.op("hint")

    // Use custom window switcher.
    // "tab:alt": slate.op("switch"),
});

/* ----------------------- */
/* APPLESCRIPT KEYBINDINGS */
/* ----------------------- */

bindApplescript("`:cmd;ctrl;alt", "open_finder.scpt");
bindApplescript("return:cmd;ctrl;alt", "open_iterm.scpt");
bindApplescript("e:cmd;ctrl;alt", "open_emacs.scpt");
bindApplescript("w:cmd;ctrl;alt", "open_google_chrome.scpt");
bindApplescript("w:shift;cmd;ctrl;alt", "open_google_chrome_incognito.scpt");

/* ------------------ */
/* LAYOUT KEYBINDINGS */
/* ------------------ */

// Maximize the window on the current screen.
slate.bind("m:cmd;ctrl;alt", slate.op("move", {
    "x": "screenOriginX",
    "y": "screenOriginY",
    "width": "screenSizeX",
    "height": "screenSizeY"
}));

// Resize the window so it's 2/3 the width of the screen and throw it to the
// left or right.
slate.bind("u:cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
    var width = screenCoords.width * 2 / 3,
        height = screenCoords.height,
        positions = [
            {
                "x": screenCoords.x1,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y1
            }
        ];

    return {
        "width": width,
        "height": height,
        "positions": positions
    };
}));

// Resize the window so it's 1/3 the width of the screen and move it between the
// 3 columns.
slate.bind("i:cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
    var width = screenCoords.width / 3,
        height = screenCoords.height,
        positions = [
            {
                "x": screenCoords.x1,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x1 + width,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y1
            }
        ];

    return {
        "width": width,
        "height": height,
        "positions": positions
    };
}));

// Resize the window so it's 1/3 the width of the screen and 1/2 the height of
// the screen, and move it between the four corners.
slate.bind("o:cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
    var width = screenCoords.width / 3,
        height = screenCoords.height / 2,
        positions = [
            {
                "x": screenCoords.x1,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y2 - height
            },
            {
                "x": screenCoords.x1,
                "y": screenCoords.y2 - height
            }
        ];

    return {
        "width": width,
        "height": height,
        "positions": positions
    };
}));

// Resize the window so it's 1/2 the width of the screen and throw it to the
// left or right.
slate.bind("u:shift;cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
    var width = screenCoords.width / 2,
        height = screenCoords.height,
        positions = [
            {
                "x": screenCoords.x1,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y1
            }
        ];

    return {
        "width": width,
        "height": height,
        "positions": positions
    };
}));

// Resize the window so it's 1/2 the width of the screen and 1/2 the height of
// the screen, and move it between the four corners.
slate.bind("o:shift;cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
    var width = screenCoords.width / 2,
        height = screenCoords.height / 2,
        positions = [
            {
                "x": screenCoords.x1,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y1
            },
            {
                "x": screenCoords.x2 - width,
                "y": screenCoords.y2 - height
            },
            {
                "x": screenCoords.x1,
                "y": screenCoords.y2 - height
            }
        ];

    return {
        "width": width,
        "height": height,
        "positions": positions
    };
}));

/* ---------------- */
/* HELPER FUNCTIONS */
/* ---------------- */

/**
 * Binds a keyboard shortcut to execute the specified applescript name. All
 * scripts are assumed to be relative to the ~/.scripts directory.
 */
function bindApplescript(shortcut, script_name) {
    slate.bind(shortcut, slate.op("shell", {
        "command": "/usr/bin/osascript " + script_name,
        "wait": true,
        "path": "~/.scripts"
    }));
}

/**
 * Helper function to define a new placement cycle.
 *
 * A "placement cycle" is a sequential list of valid placements for windows. A
 * "placement" consists of both a target width/height for the window as well as
 * the coordinates for its target position. Windows can move sequentially
 * through this list of placements, cycling back to the beginning once they
 * reach the end of the list.
 *
 * This function takes a single function argument that's expected to return a
 * dictionary describing the valid placements in the cycle.
 *
 * @param placementsLoader, func: a function that returns a dictionary describing
 *     the valid placement in the cycle.
 */
function cycleBuilder(placementsLoader) {
    // The logic for moving windows to the next placement in the cycle must run
    // in a free-form function. However, the "undo" event doesn't support
    // movements from free-form functions that were bound directly to
    // keystrokes. As a work-around, we can wrap the free-form function in a
    // "chain" operation, and bind the keystroke to the "chain" operation
    // instead.
    return slate.operation("chain", {
        "operations": [
            function(win) {
                var screenRect = win.screen().visibleRect(),
                    screenCoords = rectToCoords(screenRect),
                    winCoords = rectToCoords(win.rect()),
                    placementDict = placementsLoader(screenCoords),
                    nextPlacementParams = getNextPlacement(
                        winCoords,
                        placementDict.width,
                        placementDict.height,
                        placementDict.positions);

                win.doOperation(slate.op("move", nextPlacementParams));
            }
        ]
    });
}

/**
 * Determines where a window should be placed next given its current
 * coordinates.
 */
function getNextPlacement(winCoords, width, height, positions) {
    var nextPosIx = 0, posCoords, i;

    for (i = 0; i < positions.length; i++) {
        posCoords = {
            "x1": positions[i].x,
            "y1": positions[i].y,
            "x2": positions[i].x + width,
            "y2": positions[i].y + height
        };

        // If the window is already in the current position, then move it to the
        // next position in the list.
        if (isCoordsExactMatch(winCoords, posCoords)) {
            nextPosIx = (i + 1) % positions.length;
            break;
        }
        // Otherwise, if the window is in approximately in the current position,
        // then move it such that it's exactly in the current position.
        else if (isCoordsApproxMatch(winCoords, posCoords)) {
            nextPosIx = i;
            break;
        }
    }

    return {
        "x": positions[nextPosIx].x,
        "y": positions[nextPosIx].y,
        "width": width,
        "height": height
    };
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
 *         "y2": int,
 *         "width": int,
 *         "height": int,
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
        "y2": rectObj.y + rectObj.height,
        "width": rectObj.width,
        "height": rectObj.height
    };
}

/**
 * Returns true if the given coords objects are "exact" matches.
 *
 * We consider the given coordinates to be "exact" matches if each of the four
 * corresponding coordinate values are within 10 pixels.
 */
function isCoordsExactMatch(coords1, coords2) {
    return (isApprox(coords1.x1, coords2.x1) &&
            isApprox(coords1.y1, coords2.y1) &&
            isApprox(coords1.x2, coords2.x2) &&
            isApprox(coords1.y2, coords2.y2));
}

/**
 * Returns true if the given coordinate objects are an approximate match.
 *
 * We define an approximate match as one where at least half of the area of the
 * first coordinate object is within the bounds of the second coordinate object.
 */
function isCoordsApproxMatch(coords1, coords2) {
    var overlapCoords = {
        "x1": Math.max(coords1.x1, coords2.x1),
        "y1": Math.max(coords1.y1, coords2.y1),
        "x2": Math.min(coords1.x2, coords2.x2),
        "y2": Math.min(coords1.y2, coords2.y2)
    };

    if (overlapCoords.x2 - overlapCoords.x1 < 0 ||
        overlapCoords.y2 - overlapCoords.y1 < 0) {
        return false;
    }

    return (getArea(overlapCoords) / getArea(coords1)) > 0.5;
}

/**
 * Returns the area of the given coords object.
 */
function getArea(coords) {
    return (coords.x2 - coords.x1) * (coords.y2 - coords.y1);
}

/**
 * Return true if the two value are within a certain range of each other.
 *
 * The default tolerance is set to 20 units. Here, a unit corresponds to a
 * pixel.
 */
function isApprox(value1, value2, tolerance) {
    tolerance = tolerance || 20;
    return Math.max(value1, value2) - Math.min(value1, value2) < tolerance;
};
