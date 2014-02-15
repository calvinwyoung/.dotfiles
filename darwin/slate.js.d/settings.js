var settings = (function() {
    slate.configAll({
        "defaultToCurrentScreen": true,
        "checkDefaultsOnLoad": true,

        // When using the `focus` command to focus on a new window, keep expanding
        // the search rectangle by this number of pixels until a window is found.
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

    return {};
}());
