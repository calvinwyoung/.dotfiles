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
    "j:cmd;ctrl;alt": function(win) {
        focusNextWindow(win, false);
    },
    "k:cmd;ctrl;alt": function(win) {
        focusNextWindow(win, true);
    },
    "0:shift,cmd;ctrl;alt": function(win) {
        BASE_WINDOW_FOCUS_DIRECTIONS.toggle(win.screen().id());
    },
    "[:cmd;ctrl;alt": function(win) {
        focusNextScreen(win, true);
    },
    "]:cmd;ctrl;alt": function(win) {
        focusNextScreen(win, false);
    },

    // Push windows around
    "j:shift;cmd;ctrl;alt": slate.op("push", {"direction": "down"}),
    "k:shift;cmd;ctrl;alt": slate.op("push", {"direction": "up"}),
    "h:shift;cmd;ctrl;alt": slate.op("push", {"direction": "left"}),
    "l:shift;cmd;ctrl;alt": slate.op("push", {"direction": "right"}),

    // Throw windows between screens.
    "[:shift;cmd;ctrl;alt": slate.op("throw", {"screen": "left"}),
    "]:shift;cmd;ctrl;alt": slate.op("throw", {"screen": "right"}),

    // Undo the last movement operation
    "/:cmd;ctrl;alt": slate.op("undo"),

    // Relaunch slate
    "r:shift;cmd;ctrl;alt": slate.op("relaunch"),

    // Show window hints.
    "space:cmd;ctrl;alt": slate.op("hint")

    // Use custom window switcher.
    // "tab:alt": slate.op("switch"),
});

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
slate.bind("n:cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
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
slate.bind("n:shift;cmd;ctrl;alt", cycleBuilder(function(screenCoords) {
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
/* GLOBAL VARIABLES */
/* ---------------- */

var BASE_WINDOW_FOCUS_DIRECTIONS = (function() {
    // This object maps a screen ID to the window focus direction for that
    // screen.
    var screenFocusDirectionMap = {};

    // The default focus direction for each screen. A positive value denotes
    // clockwise, and a negative value denotes counter-clockwise.
    var defaultFocusDirection = 1;

    return {
        /**
         * Gets the base focus direction for the given screen.
         *
         * @param screenID, int: the ID of the screen whose focus direction will
         *     be returned
         *
         * @returns int: the base focus direction of the given screen
         */
        get: function(screenID) {
            if (!(screenID in screenFocusDirectionMap)) {
                screenFocusDirectionMap[screenID] = defaultFocusDirection;
            }
            return screenFocusDirectionMap[screenID];
        },

        /**
         * Toggles the focus direction for the given screen.
         *
         * If the base focus direction for the screen is currently 1 (i.e.,
         * clockwise), then it switches it to -1 (i.e., counter-clockwise).
         *
         * @param screenID, int: the ID of the screen whose focus direction will
         *     be toggled
         */
        toggle: function(screenID) {
            var curFocusDirection = this.get(screenID);
            screenFocusDirectionMap[screenID] = -1 * curFocusDirection;
        }
    };
})();

// NOTE: This doesn't work yet. It seems that you can't save the reference to a
// window and then use it to call ``.focus()`` later. Maybe I'll get this
// working some day...
var LAST_FOCUSED_WINDOW = (function() {
    var screenWindowMap = {};

    slate.on("windowFocused", function(event, win) {
        screenWindowMap[win.screen().id()] = win;
    });

    return {
        get: function (screenID) {
            return screenWindowMap[screenID];
        }
    };
})();

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
 * Given a window object, focuses on the next window in the specified direction.
 *
 * If ``reverse`` is true, then focuses on the previous window instead.
 *
 * @param win, window: a reference to the currently focused window
 * @param reverse, bool: if true, then moves focus in the same direction as the
 *     base focus direction, otherwise moves focus in the opposite direction
 */
function focusNextWindow(win, reverse) {
    // If no window is defined, then just return early.
    if (_.isUndefined(win)) {
        return;
    }

    var screenWindowsMap = getScreenWindows(),
        curScreenID = win.screen().id(),
        screenWindows = screenWindowsMap[curScreenID],
        screenCenter = getRectCenter(win.screen().visibleRect()),
        direction = BASE_WINDOW_FOCUS_DIRECTIONS.get(curScreenID);

    // If there aren't any other windows on this screen, then return early.
    if (screenWindows.length === 0) {
        return;
    }

    // This value should be "false" if not specified.
    reverse = _.isUndefined(reverse) ? false : reverse;
    if (reverse) {
        direction *= -1;
    }

    // Sort windows clockwise.
    screenWindows.sort(function(a, b) {
        // Get the center points for the two windows.
        var aCenter = getRectCenter(a.rect()),
            bCenter = getRectCenter(b.rect()),
            result = null;

        // Compute the cross product of vectors (center -> a) x (center -> b).
        var det = ((aCenter.x - screenCenter.x) * (bCenter.y - screenCenter.y) -
                   (bCenter.x - screenCenter.x) * (aCenter.y - screenCenter.y));

        // If the cross product is positive, then windows a and b form a
        // clockwise rotation around the screen center.
        result = -det;

        if (result === 0) {
            // Points a and b are on the same line from the center check which
            // point is closer to the screen center.
            var aDist = (Math.pow(aCenter.x - screenCenter.x, 2) +
                         Math.pow(aCenter.y - screenCenter.y, 2));
            var bDist = (Math.pow(bCenter.x - screenCenter.x, 2) +
                         Math.pow(bCenter.y - screenCenter.y, 2));

            // If window a is farther from the center, then it should precede b.
            result = bDist - aDist;
        }

        if (result === 0) {
            result = getWindowFingerprint(a) < getWindowFingerprint(b) ? -1 : 1;
        }

        return result * direction;
    });

    // Get the ID of the current window in the sorted window list.
    var curWindowIx;
    for (curWindowIx = 0; curWindowIx < screenWindows.length; curWindowIx++) {
        if (isWindowsMatch(screenWindows[curWindowIx], win)) {
            break;
        }
    }

    // Focus on the next window.
    screenWindows[mod(curWindowIx + 1, screenWindows.length)].focus();
}

/**
 * Moves focus to a window on the next screen.
 *
 * A ``reverse`` argument can be provided to reverse the focus direction.
 *
 * @param win, window: a reference to the currently focused window
 * @param reverse, bool: if true, then moves focus to the next screen, otherwise
 *     moves focus to the previous screen
 */
function focusNextScreen(win, reverse) {
    // This value should be "false" if not specified.
    reverse = _.isUndefined(reverse) ? false : reverse;

    var screenWindowsMap = getScreenWindows(),
        curScreenID = win.screen().id(),
        direction = reverse ? -1 : 1,
        nextScreenID = mod(curScreenID + direction, slate.screenCount());

    if (screenWindowsMap[nextScreenID].length > 0) {
        screenWindowsMap[nextScreenID][0].focus();
    }
}

/**
 * Returns a mapping between each screen ID and each window on that screen.
 *
 * @returns, dict: dictionary mapping each screen ID to a list of windows on
 *     that screen
 */
function getScreenWindows() {
    var screenWindowsMap = {};

    slate.eachApp(function(app) {
        app.eachWindow(function(win) {
            var screenID = win.screen().id();
            if (!screenWindowsMap[screenID]) {
                screenWindowsMap[screenID] = [];
            }

            if (!win.isMinimizedOrHidden() && win.title()) {
                screenWindowsMap[screenID].push(win);
            }
        });
    });

    return screenWindowsMap;
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
function rectToCoords(rect) {
    return {
        "x1": rect.x,
        "y1": rect.y,
        "x2": rect.x + rect.width,
        "y2": rect.y + rect.height,
        "width": rect.width,
        "height": rect.height
    };
}

/**
 * Returns the center point of the given rect object.
 */
function getRectCenter(rect) {
    return {
        "x": rect.x + rect.width / 2,
        "y": rect.y + rect.height / 2
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

/**
 * Returns the fingerpring for a given window object.
 *
 * @param win: a window object
 * @returns, str: the string fingerprint for the given window object.
 */
function getWindowFingerprint(win) {
    var rect = win.rect();
    return (win.pid() + ":" + win.title() + ":" + rect.x + ":" + rect.y + ":" +
            rect.width + ":" + rect.height);
}

/**
 * Returns true if the two window arguments refer to the same window on the
 * screen.
 *
 * This method exists because there doesn't seem to be an obvious way of
 * otherwise uniquely identifying windows.
 *
 * @returns true if the given window objects refer to the same window
 */
function isWindowsMatch(win1, win2) {
    return getWindowFingerprint(win1) === getWindowFingerprint(win2);
}

/**
 * Performs a modulus operation on the given dividend and divisor.
 */
function mod(dividend, divisor) {
    return ((dividend % divisor) + divisor) % divisor;
}
