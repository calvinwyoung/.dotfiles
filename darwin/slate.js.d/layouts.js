var layouts = (function() {
    var self = {},
        LEFT_SCREEN = "0",
        RIGHT_SCREEN = "1";

    self.dualMonitors = slate.layout("dualMonitors", {
        "_after_": {
            "operations": [
                function(win) {
                    helpers.setWindowFocusDirection(LEFT_SCREEN, -1);
                    helpers.setWindowFocusDirection(RIGHT_SCREEN, 1);
                }
            ]
        },

        "Google Chrome": {
            "operations": [
                slate.op("move", {
                    "screen": LEFT_SCREEN,
                    "x": "screenOriginX+(screenSizeX/3)",
                    "y": "screenOriginY",
                    "width": "screenSizeX*2/3",
                    "height": "screenSizeY"
                })
            ]
        },

        "Emacs": {
            "sort-title": true,
            "operations": [
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY"
                }),
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX+(screenSizeX/3)",
                    "y": "screenOriginY",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY"
                })
            ]
        },

        "iTerm": {
            "sort-title": true,
            "operations": [
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX+(screenSizeX*2/3)",
                    "y": "screenOriginY",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY/2"
                }),
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX+(screenSizeX*2/3)",
                    "y": "screenOriginY+(screenSizeY/2)",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY/2"
                }),
                slate.op("move", {
                    "screen": LEFT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY+(screenSizeY/2)",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY/2"
                })
            ]
        },

        "HipChat": {
            "operations": [
                slate.op("move", {
                    "screen": LEFT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY/2"
                })
            ]
        }
    });

    return self;
}());
