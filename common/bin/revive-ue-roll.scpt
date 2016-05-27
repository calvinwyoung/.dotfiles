#!/usr/bin/osascript
#
# When the UE Roll is attached to multiple devices, it can sometimes get locked to
# another device even if that other device isn't playing audio. This script
# attempts to regain control of the UE Roll for this Macbook by temporarily
# switching the sound output away from the UE Roll, and then resetting the UE Roll
# as the primary output device.
#
# Usage: ./revive-ue-roll.scpt

tell application "System Preferences"
    reveal anchor "output" of pane id "com.apple.preference.sound"
    activate

    tell application "System Events"
        tell process "System Preferences"
            tell window "Sound"
                tell tab group 1
                    tell scroll area 1
                        tell table 1
                            select (row 1 whose value of text field 1 is "Internal Speakers")

                            delay 3

                            select (row 1 whose value of text field 1 is "UE ROLL")
                        end tell
                    end tell
                end tell
            end tell
        end tell
    end tell

    quit
end tell
