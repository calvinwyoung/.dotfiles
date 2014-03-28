var helpers = (function() {
    var self = {},

        // The default focus direction for each screen. A positive value denotes
        // clockwise, and a negative value denotes counter-clockwise.
        DEFAULT_FOCUS_DIRECTION = 1,

        // Maps a screen ID to the window focus direction for that screen.
        SCREEN_FOCUS_DIRECTIONS = {},

        // Maps an app PID to its pre-maximized size/position, stored as a
        // window `rect` object. This is used to revert windows back to their
        // original sizes after maximizing them.
        PRE_MAXIMIZED_POSITIONS = {},

        // Stores the fingerprint of the last used window for each screen.
        LAST_FOCUSED_WIN_BY_SCREEN = {};

    /**
     * Custom operation that lets us maximize a window, and then revert it back
     * to its original position.
     *
     * Before maximizing a window, this operation records an entry in the
     * `PRE_MAXIMIZED_POSITIONS` dictionary that maps the window's app PID to its
     * original size/position. We use app PIDs because we can't uniquely
     * identify windows, and app PIDs are the next best thing. A consequence of
     * this is that if we maximize two windows from the same application, the
     * position for the second window will overwrite the position of the
     * first. This is not ideal, but the UX is fine in practice.
     */
    self.toggleMaximizedOp = function() {
        return slate.operation("chain", {
            "operations": [
                function(win) {
                    var screenRect = win.screen().visibleRect(),
                        screenCoords = self.rectToCoords(screenRect),
                        winCoords = self.rectToCoords(win.rect());

                    if (self.isCoordsApproxMatch(winCoords, screenCoords)) {
                        if (PRE_MAXIMIZED_POSITIONS[win.pid()]) {
                            win.doOperation(
                                slate.op("move", PRE_MAXIMIZED_POSITIONS[win.pid()])
                            );
                        }
                    } else {
                        PRE_MAXIMIZED_POSITIONS[win.pid()] = win.rect();
                        win.doOperation(slate.op("move", screenRect));
                    }
                }
            ]
        });
    },

    /**
     * Helper function to define a new placement cycle.
     *
     * A "placement cycle" is a list of valid window placements, where a
     * "placement" consists of a target size and position. Windows cycle through
     * this list of placements, wrapping back to the beginning once they reach
     * the end of the list.
     *
     * This function takes a single function argument that's expected to return
     * a dictionary describing the valid placements in the cycle.
     *
     * @param placementsLoader, func: a function that returns a dictionary
     *    describing the valid placement in the cycle.
     */
    self.cycleBuilder = function(placementsLoader) {
        // The logic for moving windows to the next placement in the cycle must
        // run in a free-form function. However, the "undo" event doesn't
        // support movements from free-form functions that were bound directly
        // to keystrokes. As a work-around, we can wrap the free-form function
        // in a "chain" operation, and bind the keystroke to the "chain"
        // operation instead.
        return slate.operation("chain", {
            "operations": [
                function(win) {
                    var screenRect = win.screen().visibleRect(),
                        screenCoords = self.rectToCoords(screenRect),
                        winCoords = self.rectToCoords(win.rect()),
                        placementDict = placementsLoader(screenCoords),
                        nextPlacementParams = self.getNextPlacement(
                            winCoords,
                            placementDict.width,
                            placementDict.height,
                            placementDict.positions);

                    win.doOperation(slate.op("move", nextPlacementParams));
                }
            ]
        });
    };

    /**
     * Determines where a window should be placed next given its coordinates
     * current.
     */
    self.getNextPlacement = function(winCoords, width, height, positions) {
        var nextPosIx = 0, posCoords, i;

        for (i = 0; i < positions.length; i++) {
            posCoords = {
                "x1": positions[i].x,
                "y1": positions[i].y,
                "x2": positions[i].x + width,
                "y2": positions[i].y + height
            };

            // If the window is already in the current position, then move it to
            // the next position in the list.
            if (self.isCoordsApproxMatch(winCoords, posCoords)) {
                nextPosIx = (i + 1) % positions.length;
                break;
            }
            // Otherwise, if the window mostly overlaps the current target
            // position, then move it so that it's exactly in the current target
            // position.
            else if (self.isCoordsOverlapMatch(winCoords, posCoords)) {
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
    };

    /**
     * Given a window object, focuses on the next window in the specified
     * direction.
     *
     * If ``reverse`` is true, then focuses on the previous window instead.
     *
     * @param win, window: a reference to the currently focused window
     * @param reverse, bool: if true, then moves focus in the same direction as
     *    the base focus direction, otherwise moves focus in the opposite
     *    direction
     */
    self.focusNextWindow = function(win, reverse) {
        // If no window is defined, then just return early.
        if (_.isUndefined(win)) {
            return;
        }

        var screenWindowsMap = self.getScreenWindows(),
            curScreenID = win.screen().id(),
            screenWindows = screenWindowsMap[curScreenID],
            screenCenter = self.getRectCenter(win.screen().visibleRect()),
            direction = self.getWindowFocusDirection(curScreenID);

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
            var aCenter = self.getRectCenter(a.rect()),
                bCenter = self.getRectCenter(b.rect()),
                result = null;

            // Compute the cross product of vectors (center -> a) x (center -> b).
            var det = (
                (aCenter.x - screenCenter.x) * (bCenter.y - screenCenter.y) -
                (bCenter.x - screenCenter.x) * (aCenter.y - screenCenter.y));

            // If the cross product is positive, then windows a and b form a
            // clockwise rotation around the screen center.
            result = -det;

            if (result === 0) {
                // Points a and b are on the same line from the center check
                // which point is closer to the screen center.
                var aDist = (Math.pow(aCenter.x - screenCenter.x, 2) +
                             Math.pow(aCenter.y - screenCenter.y, 2));
                var bDist = (Math.pow(bCenter.x - screenCenter.x, 2) +
                             Math.pow(bCenter.y - screenCenter.y, 2));

                // If window a is farther from the center, then it should
                // precede b.
                result = bDist - aDist;
            }

            if (result === 0) {
                if (self.getWindowFingerprint(a) < self.getWindowFingerprint(b)) {
                    result = -1;
                } else {
                    result = 1;
                }
            }

            return result * direction;
        });

        // Get the ID of the current window in the sorted window list.
        var curWindowIx;
        for (curWindowIx = 0; curWindowIx < screenWindows.length; curWindowIx++) {
            if (self.isWindowsMatch(screenWindows[curWindowIx], win)) {
                break;
            }
        }

        // Focus on the next window.
        screenWindows[self.mod(curWindowIx + 1, screenWindows.length)].focus();
    };

    /**
     * Moves focus to a window on the next screen.
     *
     * A ``reverse`` argument can be provided to reverse the focus direction.
     *
     * @param win, window: a reference to the currently focused window
     * @param reverse, bool: if true, then moves focus to the next screen,
     *     otherwise moves focus to the previous screen
     */
    self.focusNextScreen = function(win, reverse) {
        // This value should be "false" if not specified.
        reverse = _.isUndefined(reverse) ? false : reverse;

        var screenWindowsMap = self.getScreenWindows(),
            curScreenID = _.isUndefined(win) ?
                _.keys(screenWindowsMap)[0] :
                win.screen().id(),
            direction = reverse ? -1 : 1,
            nextScreenID = self.mod(curScreenID + direction,
                                    slate.screenCount());

        if (screenWindowsMap[nextScreenID].length > 0) {
            // First, save the currently focused window's fingerprint for this
            // screen.
            LAST_FOCUSED_WIN_BY_SCREEN[curScreenID] =
                self.getWindowFingerprint(win);

            // Look for a window that matches the last focused window for the
            // next screen.
            var nextWindowFingerprint = LAST_FOCUSED_WIN_BY_SCREEN[nextScreenID],
                nextWindow = _.find(screenWindowsMap[nextScreenID], function(win) {
                    return self.getWindowFingerprint(win) === nextWindowFingerprint;
                });

            // If a window wasn't found, the default to the first window on that
            // screen.
            if (!nextWindow) {
                nextWindow = screenWindowsMap[nextScreenID][0];
            }

            nextWindow.focus();
        }
    };

    /**
     * Gets the base focus direction for the given screen.
     *
     * @param screenID, int: the ID of the screen whose focus direction will
     *     be returned
     *
     * @returns int: the base focus direction of the given screen
     */
    self.getWindowFocusDirection = function(screenID) {
        if (!(screenID in SCREEN_FOCUS_DIRECTIONS)) {
            SCREEN_FOCUS_DIRECTIONS[screenID] = DEFAULT_FOCUS_DIRECTION;
        }
        return SCREEN_FOCUS_DIRECTIONS[screenID];
    };

    /**
     * Sets the base focus direction for the given screen.
     *
     * @param screenID, int: the ID of the screen whose focus direction will
     *     be returned
     * @param direction, int: the focus direction to set for the given screen
     */
    self.setWindowFocusDirection = function(screenID, direction) {
        SCREEN_FOCUS_DIRECTIONS[screenID] = direction;
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
    self.toggleWindowFocusDirection = function(screenID) {
        self.setWindowFocusDirection(
            screenID,
            -self.getWindowFocusDirection(screenID));
    };

    /**
     * Returns a mapping between each screen ID and each window on that screen.
     *
     * @returns, dict: dictionary mapping each screen ID to a list of windows on
     *     that screen
     */
    self.getScreenWindows= function() {
        var screenWindowsMap = {};

        slate.eachScreen(function(screen) {
            screenWindowsMap[screen.id()] = [];
        });

        slate.eachApp(function(app) {
            app.eachWindow(function(win) {
                if (!win.isMinimizedOrHidden() && win.title()) {
                    screenWindowsMap[win.screen().id()].push(win);
                }
            });
        });

        return screenWindowsMap;
    };

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
     * where (x1, y1) is the coordinate of the top-left corner and (x2, y2) is
     * the coordinate of the bottom-right corner.
     */
    self.rectToCoords = function(rect) {
        return {
            "x1": rect.x,
            "y1": rect.y,
            "x2": rect.x + rect.width,
            "y2": rect.y + rect.height,
            "width": rect.width,
            "height": rect.height
        };
    };

    /**
     * Returns the center point of the given rect object.
     */
    self.getRectCenter = function(rect) {
        return {
            "x": rect.x + rect.width / 2,
            "y": rect.y + rect.height / 2
        };
    };

    /**
     * Returns true if the given coords objects are approximate matches.
     *
     * Please refer to the `isApprox` match for implementation details about how
     * we determine whether two coordinates are approximate matches.
     */
    self.isCoordsApproxMatch = function(coords1, coords2) {
        return (self.isApprox(coords1.x1, coords2.x1) &&
                self.isApprox(coords1.y1, coords2.y1) &&
                self.isApprox(coords1.x2, coords2.x2) &&
                self.isApprox(coords1.y2, coords2.y2));
    };

    /**
     * Returns true if the given coordinate objects are mostly overlapping.
     *
     * We define an approximate match as one where at least half of the area of
     * the first coordinate object is within the bounds of the second coordinate
     * object.
     */
    self.isCoordsOverlapMatch = function(coords1, coords2) {
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

        return (self.getArea(overlapCoords) / self.getArea(coords1)) > 0.5;
    };

    /**
     * Returns the area of the given coords object.
     */
    self.getArea = function(coords) {
        return (coords.x2 - coords.x1) * (coords.y2 - coords.y1);
    };

    /**
     * Return true if the two value are within a certain range of each other.
     *
     * The default tolerance is set to 20 units. Here, a unit corresponds to a
     * pixel.
     */
    self.isApprox = function(value1, value2, tolerance) {
        tolerance = tolerance || 20;
        return Math.max(value1, value2) - Math.min(value1, value2) < tolerance;
    };

    /**
     * Returns the fingerprint for a given window object.
     *
     * This method simply concatenates the window PID with its coordinates. Note
     * that we can't use the window title because it can change very frequently
     * for some apps (e.g., Chrome).
     *
     * @param win: a window object
     * @returns, str: the string fingerprint for the given window object.
     */
    self.getWindowFingerprint = function(win) {
        var rect = win.rect();
        return (win.pid() + ":" +
                rect.x + ":" + rect.y + ":" + rect.width + ":" + rect.height);
    };

    /**
     * Returns true if the two window arguments refer to the same window on the
     * screen.
     *
     * This method exists because there doesn't seem to be an obvious way of
     * otherwise uniquely identifying windows.
     *
     * @returns true if the given window objects refer to the same window
     */
    self.isWindowsMatch = function(win1, win2) {
        return (self.getWindowFingerprint(win1) ===
                self.getWindowFingerprint(win2));
    };

    /**
     * Performs a modulus operation on the given dividend and divisor.
     */
    self.mod = function(dividend, divisor) {
        return ((dividend % divisor) + divisor) % divisor;
    };

    return self;
}());
