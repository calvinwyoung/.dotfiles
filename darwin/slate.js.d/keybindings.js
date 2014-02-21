var keybindings = (function() {
    /* ------------------- */
    /* GENERAL KEYBINDINGS */
    /* ------------------- */

    slate.bindAll({
        // Change focus.
        "j:cmd;ctrl;alt": function(win) {
            helpers.focusNextWindow(win, false);
        },
        "k:cmd;ctrl;alt": function(win) {
            helpers.focusNextWindow(win, true);
        },
        "0:shift,cmd;ctrl;alt": function(win) {
            helpers.toggleWindowFocusDirection(win.screen().id());
        },
        "[:cmd;ctrl;alt": function(win) {
            helpers.focusNextScreen(win, true);
        },
        "]:cmd;ctrl;alt": function(win) {
            helpers.focusNextScreen(win, false);
        },

        // Push windows around.
        "j:shift;cmd;ctrl;alt": slate.op("push", {"direction": "down"}),
        "k:shift;cmd;ctrl;alt": slate.op("push", {"direction": "up"}),
        "h:shift;cmd;ctrl;alt": slate.op("push", {"direction": "left"}),
        "l:shift;cmd;ctrl;alt": slate.op("push", {"direction": "right"}),

        // Throw windows between screens.
        "[:shift;cmd;ctrl;alt": slate.op("throw", {"screen": "left"}),
        "]:shift;cmd;ctrl;alt": slate.op("throw", {"screen": "right"}),

        // Load our custom dual monitor layout.
        "padEnter:cmd;ctrl;alt": slate.op("layout", {"name": "dualMonitors"}),

        // Undo the last movement operation
        "/:cmd;ctrl;alt": slate.op("undo"),

        // Relaunch slate.
        "r:shift;cmd;ctrl;alt": slate.op("relaunch"),

        // Show window hints.
        "space:cmd;ctrl;alt": slate.op("hint")
    });

    /* ------------------ */
    /* LAYOUT KEYBINDINGS */
    /* ------------------ */

    // Maximize the window on the current screen.
    slate.bind("m:cmd;ctrl;alt", helpers.toggleMaximizedOp());

    // Resize the window so it's 2/3 the width of the screen and throw it to the
    // left or right.
    slate.bind("u:cmd;ctrl;alt", helpers.cycleBuilder(function(screenCoords) {
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
    slate.bind("i:cmd;ctrl;alt", helpers.cycleBuilder(function(screenCoords) {
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
    slate.bind("n:cmd;ctrl;alt", helpers.cycleBuilder(function(screenCoords) {
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
    slate.bind("u:shift;cmd;ctrl;alt", helpers.cycleBuilder(function(screenCoords) {
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
    slate.bind("n:shift;cmd;ctrl;alt", helpers.cycleBuilder(function(screenCoords) {
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

    return {};
}());
